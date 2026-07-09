module Infra.Adapters.Analyser
  ( Device,
    Source (..),
    scope,
  )
where

import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Database.PostgreSQL.LibPQ qualified as Pq
import Fx
import Hasql.Connection.Settings qualified
import Hasql.Errors qualified
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Hasql.Session qualified
import HasqlDev qualified
import Infra.Adapters.Analyser.Embeddings.Sessions qualified as Embeddings.Sessions
import Infra.Adapters.Analyser.Scopes.Testcontainers qualified as Testcontainers
import Infra.Adapters.Analyser.Sessions qualified as Sessions
import Infra.Adapters.Analyser.Sessions.Procedures.GetIndexes qualified as GetIndexes
import Interpreters.Observing qualified as Observing
import Logic.Capabilities.IndexCatalog (LoadsIndexes (..))
import Logic.Capabilities.Migrations (ExecutesMigrations (..))
import Logic.Capabilities.QueryAnalysis (InfersQueryTypes (..))
import Logic.Capabilities.SeqScanExplain (ExplainsQuery (..))
import Logic.Domain.Report qualified as Report
import TestcontainersPostgresql qualified
import Utils.Prelude
import Utils.Text qualified

-- | A connection pool to the PostgreSQL instance used for query analysis.
newtype Device = Device Hasql.Pool.Pool

-- | Selects the PostgreSQL backend for analysis.
data Source
  = -- | Start a fresh Docker container using the given image tag.
    DockerSource {postgresTag :: Text}
  | -- | Connect to a running PostgreSQL server.
    -- A temporary database is created for analysis and dropped on cleanup.
    RunningServerSource {connectionUrl :: Text, targetMajorVersion :: Int}

-- | Acquire a 'Device' for the given 'Source', reporting progress via the
-- observer, and release it (dropping any temporary database or container)
-- when the enclosing scope exits.
scope :: Source -> (Observing.Observation -> IO ()) -> Fx.Scope Report.Report Device
scope source observe = case source of
  DockerSource {postgresTag} -> scopeViaDocker postgresTag observe
  RunningServerSource {connectionUrl, targetMajorVersion} ->
    scopeViaRunningServer connectionUrl targetMajorVersion observe

scopeViaDocker :: Text -> (Observing.Observation -> IO ()) -> Fx.Scope Report.Report Device
scopeViaDocker postgresTag observe = do
  acquire $ runTotalIO \() -> observe (Observing.StageEntered ["Starting Container"])
  (host, port) <-
    first
      adaptTestcontainersError
      ( Testcontainers.testContainer
          ( TestcontainersPostgresql.setup
              TestcontainersPostgresql.Config
                { tagName = postgresTag,
                  auth = TestcontainersPostgresql.TrustAuth,
                  forwardLogs = False
                }
          )
      )
  acquire $ runTotalIO \() -> observe (Observing.StageExited ["Starting Container"] 0.9)
  acquire $ runTotalIO \() -> observe (Observing.StageEntered ["Connecting"])
  pool <-
    scopePool
      ( Hasql.Pool.Config.settings
          [ Hasql.Pool.Config.size 100,
            Hasql.Pool.Config.staticConnectionSettings
              ( mconcat
                  [ Hasql.Connection.Settings.hostAndPort host port,
                    Hasql.Connection.Settings.user "postgres",
                    Hasql.Connection.Settings.password "",
                    Hasql.Connection.Settings.dbname "postgres"
                  ]
              )
          ]
      )
  acquire $ runTotalIO \() -> observe (Observing.StageExited ["Connecting"] 0.1)
  pure (Device pool)
  where
    adaptTestcontainersError :: SomeException -> Report.Report
    adaptTestcontainersError err =
      Report.Report
        { path = ["testcontainers"],
          message = Text.pack (displayException err),
          suggestion = Nothing,
          details = []
        }

-- | Connect to a running server, verify its major version, create a temporary
-- analysis database, and return a device backed by a pool on that database.
-- The temporary database is dropped when the scope exits (on success or error).
scopeViaRunningServer :: Text -> Int -> (Observing.Observation -> IO ()) -> Fx.Scope Report.Report Device
scopeViaRunningServer connectionUrl targetMajorVersion observe = do
  let serverSettings = Hasql.Connection.Settings.connectionString connectionUrl

  acquire $ runTotalIO \() -> observe (Observing.StageEntered ["Connecting"])

  -- Admin pool (size 1) for version check and DB management.
  -- Its release is registered first so it runs last in LIFO cleanup.
  adminPool <-
    scopePool
      ( Hasql.Pool.Config.settings
          [ Hasql.Pool.Config.size 1,
            Hasql.Pool.Config.staticConnectionSettings serverSettings
          ]
      )

  -- Validate the server major version by reusing the admin pool connection.
  serverMajorVersion <- acquire $ runPartialIO \() -> do
    res <- Hasql.Pool.use adminPool queryVersionSession
    pure $ case res of
      Left poolErr ->
        Left
          Report.Report
            { path = [],
              message = "Failed to connect to PostgreSQL server",
              suggestion = Nothing,
              details =
                [ ("reason", Text.pack (show poolErr))
                ]
            }
      Right (Left msg) ->
        Left
          Report.Report
            { path = [],
              message = "Failed to query server version",
              suggestion = Nothing,
              details =
                [ ("reason", msg)
                ]
            }
      Right (Right n) -> Right n

  when (serverMajorVersion /= targetMajorVersion) do
    throwError
      Report.Report
        { path = [],
          message =
            "PostgreSQL server version "
              <> Text.pack (show serverMajorVersion)
              <> " does not match the project target version "
              <> Text.pack (show targetMajorVersion),
          suggestion =
            Just
              ( "Set 'postgres: "
                  <> Text.pack (show serverMajorVersion)
                  <> "' in the project file to match the running server, "
                  <> "or start a PostgreSQL "
                  <> Text.pack (show targetMajorVersion)
                  <> " server"
              ),
          details =
            [ ("server", Text.pack (show serverMajorVersion)),
              ("target", Text.pack (show targetMajorVersion))
            ]
        }

  -- Generate a UUID-based name for the temporary analysis database.
  tempDbName <- acquire $ runTotalIO \() -> do
    uuid <- UUID.V4.nextRandom
    let uuidText = Text.pack (UUID.toString uuid)
    pure ("pgenie_" <> Text.map (\c -> if c == '-' then '_' else c) uuidText)

  -- Create the temporary database via the admin pool.
  acquire $ runPartialIO \() -> do
    res <- Hasql.Pool.use adminPool (Hasql.Session.script ("CREATE DATABASE " <> quoteIdent tempDbName))
    pure $ case res of
      Left poolErr ->
        Left
          Report.Report
            { path = [],
              message = "Failed to create temporary analysis database",
              suggestion = Just "Ensure the database user has the CREATEDB privilege",
              details = [("reason", Text.pack (show poolErr))]
            }
      Right () -> Right ()

  -- Register DROP DATABASE *before* creating the analysis pool so that in
  -- LIFO cleanup order it runs after the analysis pool is released.
  registerRelease $ runTotalIO \() -> do
    _ <- Hasql.Pool.use adminPool (Hasql.Session.script ("DROP DATABASE " <> quoteIdent tempDbName))
    pure ()

  -- Analysis pool on the temporary database.
  -- Its release is registered last so it runs first in LIFO cleanup.
  let analysisSettings = serverSettings <> Hasql.Connection.Settings.dbname tempDbName
  analysisPool <-
    scopePool
      ( Hasql.Pool.Config.settings
          [ Hasql.Pool.Config.size 100,
            Hasql.Pool.Config.staticConnectionSettings analysisSettings
          ]
      )

  acquire $ runTotalIO \() -> observe (Observing.StageExited ["Connecting"] 1)

  pure (Device analysisPool)
  where
    -- Session that reads the server major version via the existing connection,
    -- avoiding the need to open a separate libpq connection.
    queryVersionSession :: Hasql.Session.Session (Either Text Int)
    queryVersionSession =
      Hasql.Session.onLibpqConnection \conn -> do
        mResult <- Pq.exec conn "SELECT current_setting('server_version_num')::int / 10000"
        result <- case mResult of
          Nothing -> fmap (Left . msgOf) (Pq.errorMessage conn)
          Just res -> do
            rst <- Pq.resultStatus res
            if rst == Pq.TuplesOk
              then do
                mVal <- Pq.getvalue res (Pq.Row 0) (Pq.Col 0)
                pure $ case mVal >>= readMaybe . Text.unpack . TextEncoding.decodeUtf8Lenient of
                  Just n -> Right n
                  Nothing -> Left "Could not parse server_version_num"
              else fmap (Left . msgOf) (Pq.resultErrorMessage res)
        pure (Right result, conn)
      where
        msgOf :: Maybe ByteString -> Text
        msgOf = maybe "Unknown error" TextEncoding.decodeUtf8Lenient

    -- Double-quote a PostgreSQL identifier for safe embedding in DDL.
    quoteIdent :: Text -> Text
    quoteIdent ident = "\"" <> Text.replace "\"" "\"\"" ident <> "\""

-- | Acquire a Hasql pool for the duration of the enclosing scope and register
-- its release as a cleanup action.  Used by both Docker and running-server paths.
scopePool :: Hasql.Pool.Config.Config -> Fx.Scope Report.Report Hasql.Pool.Pool
scopePool config = do
  pool <- acquire $ runTotalIO \() -> Hasql.Pool.acquire config
  registerRelease $ runTotalIO \() -> Hasql.Pool.release pool
  pure pool

instance HasqlDev.RunsSession (Fx Device Report.Report) where
  runSession session =
    runPartialIO \(Device pool) ->
      first adaptPoolUsageError <$> Hasql.Pool.use pool session
    where
      adaptPoolUsageError :: Hasql.Pool.UsageError -> Report.Report
      adaptPoolUsageError err = case err of
        Hasql.Pool.SessionUsageError sessionErr ->
          adaptSessionError sessionErr
        otherErr ->
          Report.Report
            { path = [],
              message = Text.pack (show otherErr),
              suggestion = Nothing,
              details = []
            }

      adaptSessionError :: Hasql.Errors.SessionError -> Report.Report
      adaptSessionError sessionErr = case sessionErr of
        Hasql.Errors.ScriptSessionError sql serverErr ->
          adaptServerError sql serverErr
        Hasql.Errors.StatementSessionError _ _ sql _ _ statementErr ->
          case statementErr of
            Hasql.Errors.ServerStatementError serverErr ->
              adaptServerError sql serverErr
            otherErr ->
              Report.Report
                { path = [],
                  message = Text.pack (show otherErr),
                  suggestion = Nothing,
                  details = [("sql", sql)]
                }
        Hasql.Errors.ConnectionSessionError msg ->
          Report.Report
            { path = [],
              message = msg,
              suggestion = Nothing,
              details = []
            }
        Hasql.Errors.MissingTypesSessionError types ->
          Report.Report
            { path = [],
              message = "Missing database types",
              suggestion = Just "Ensure all referenced types exist in the database",
              details = [("types", Text.pack (show types))]
            }
        Hasql.Errors.DriverSessionError msg ->
          Report.Report
            { path = [],
              message = msg,
              suggestion = Nothing,
              details = []
            }

      adaptServerError :: Text -> Hasql.Errors.ServerError -> Report.Report
      adaptServerError sql (Hasql.Errors.ServerError errorCode message details hint position) =
        Report.Report
          { path = [],
            message = message,
            suggestion = derivedSuggestion,
            details =
              catMaybes
                [ Just ("code", errorCode),
                  Just ("sql", maybe sql (\pos -> Utils.Text.pointToLocation sql pos) position),
                  extraDetails
                ]
          }
        where
          (derivedSuggestion, extraDetails) = case errorCode of
            "23502" ->
              -- not_null_violation: the server-level details contain internal
              -- "Failing row contains ..." info which is not useful to the user.
              -- Derive an actionable suggestion from the error message instead.
              (Just (notNullViolationSuggestion message), Nothing)
            _ ->
              (hint, ("details",) <$> details)

      notNullViolationSuggestion :: Text -> Text
      notNullViolationSuggestion message =
        case Text.stripPrefix "null value in column \"" message of
          Just rest ->
            let col = Text.takeWhile (/= '"') rest
             in "Add \"" <> col <> "\" to the INSERT column list, or define a DEFAULT value for the column in the schema"
          Nothing ->
            "Add all NOT NULL columns to the INSERT statement, or define DEFAULT values for them in the schema"

instance ExecutesMigrations (Fx Device Report.Report) where
  executeMigration migrationText =
    HasqlDev.runSession (Hasql.Session.script migrationText)

instance InfersQueryTypes (Fx Device Report.Report) where
  inferQueryTypes sql =
    HasqlDev.runSession (Sessions.inferTypes sql) >>= \case
      Left err ->
        throwError (adaptAnalysisError err)
      Right (query, warnings) ->
        case Embeddings.Sessions.adaptQuery query of
          Left err ->
            throwError err
          Right queryTypes ->
            pure
              ( queryTypes,
                map adaptAnalysisError warnings
              )
    where
      adaptAnalysisError :: Sessions.Error -> Report.Report
      adaptAnalysisError err =
        Report.Report
          { path = err.location,
            message = err.reason,
            suggestion = Nothing,
            details = err.details
          }

instance ExplainsQuery (Fx Device Report.Report) where
  explainQuery sql =
    HasqlDev.runSession do
      Hasql.Session.onLibpqConnection \conn -> do
        let explainSql = TextEncoding.encodeUtf8 ("EXPLAIN (GENERIC_PLAN) " <> sql)
        maybeResult <- Pq.exec conn explainSql
        rows <- case maybeResult of
          Nothing -> pure []
          Just result -> do
            status <- Pq.resultStatus result
            case status of
              Pq.TuplesOk -> do
                numRows <- Pq.ntuples result
                forM [0 .. numRows - 1] \i ->
                  fmap (foldMap TextEncoding.decodeUtf8Lenient) (Pq.getvalue result i (Pq.Col 0))
              _ -> pure []
        pure (Right rows, conn)

instance LoadsIndexes (Fx Device Report.Report) where
  getIndexes =
    HasqlDev.runSession GetIndexes.getIndexes
