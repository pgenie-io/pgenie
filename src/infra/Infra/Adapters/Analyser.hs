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
import Hasql.Errors qualified as Hasql
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Hasql.Session qualified
import HasqlDev qualified
import Infra.Adapters.Analyser.Embeddings.Sessions qualified as Embeddings.Sessions
import Infra.Adapters.Analyser.Scopes.Testcontainers qualified
import Infra.Adapters.Analyser.Sessions qualified as Sessions
import Infra.Adapters.Analyser.Sessions.Procedures.GetIndexes qualified as GetIndexes
import Logic qualified
import TestcontainersPostgresql qualified
import Utils.Prelude
import Utils.Text qualified

newtype Device = Device Hasql.Pool.Pool

-- | Selects the PostgreSQL backend for analysis.
data Source
  = -- | Start a fresh Docker container using the given image tag.
    DockerSource {postgresTag :: Text}
  | -- | Connect to a running PostgreSQL server.
    -- A temporary database is created for analysis and dropped on cleanup.
    RunningServerSource {connectionUrl :: Text, targetMajorVersion :: Int}

scope :: Source -> (Logic.Event -> IO ()) -> Fx.Scope Logic.Report Device
scope source emitEvent = case source of
  DockerSource {postgresTag} -> scopeViaDocker postgresTag emitEvent
  RunningServerSource {connectionUrl, targetMajorVersion} ->
    scopeViaRunningServer connectionUrl targetMajorVersion emitEvent

scopeViaDocker :: Text -> (Logic.Event -> IO ()) -> Fx.Scope Logic.Report Device
scopeViaDocker postgresTag emitEvent = do
  acquire $ runTotalIO \() -> emitEvent (Logic.StageEntered ["Starting Container"])
  (host, port) <-
    first
      adaptTestcontainersError
      ( Infra.Adapters.Analyser.Scopes.Testcontainers.testContainer
          ( TestcontainersPostgresql.setup
              TestcontainersPostgresql.Config
                { tagName = postgresTag,
                  auth = TestcontainersPostgresql.TrustAuth,
                  forwardLogs = False
                }
          )
      )
  acquire $ runTotalIO \() -> emitEvent (Logic.StageExited ["Starting Container"] 0.9)
  acquire $ runTotalIO \() -> emitEvent (Logic.StageEntered ["Connecting"])
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
  acquire $ runTotalIO \() -> emitEvent (Logic.StageExited ["Connecting"] 0.1)
  pure (Device pool)
  where
    adaptTestcontainersError :: SomeException -> Logic.Report
    adaptTestcontainersError err =
      Logic.Report
        { path = ["testcontainers"],
          message = Text.pack (displayException err),
          suggestion = Nothing,
          details = []
        }

-- | Connect to a running server, verify its major version, create a temporary
-- analysis database, and return a device backed by a pool on that database.
-- The temporary database is dropped when the scope exits (on success or error).
scopeViaRunningServer :: Text -> Int -> (Logic.Event -> IO ()) -> Fx.Scope Logic.Report Device
scopeViaRunningServer connectionUrl targetMajorVersion emitEvent = do
  let serverSettings = Hasql.Connection.Settings.connectionString connectionUrl

  acquire $ runTotalIO \() -> emitEvent (Logic.StageEntered ["Connecting"])

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
          Logic.Report
            { path = [],
              message = "Failed to connect to PostgreSQL server",
              suggestion = Nothing,
              details =
                [ ("reason", Text.pack (show poolErr))
                ]
            }
      Right (Left msg) ->
        Left
          Logic.Report
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
      Logic.Report
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
          Logic.Report
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

  acquire $ runTotalIO \() -> emitEvent (Logic.StageExited ["Connecting"] 1)

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
scopePool :: Hasql.Pool.Config.Config -> Fx.Scope Logic.Report Hasql.Pool.Pool
scopePool config = do
  pool <- acquire $ runTotalIO \() -> Hasql.Pool.acquire config
  registerRelease $ runTotalIO \() -> Hasql.Pool.release pool
  pure pool

instance HasqlDev.RunsSession (Fx Device Logic.Report) where
  runSession session =
    runPartialIO \(Device pool) ->
      first adaptPoolUsageError <$> Hasql.Pool.use pool session
    where
      adaptPoolUsageError :: Hasql.Pool.UsageError -> Logic.Report
      adaptPoolUsageError err = case err of
        Hasql.Pool.SessionUsageError sessionErr ->
          adaptSessionError sessionErr
        otherErr ->
          Logic.Report
            { path = [],
              message = Text.pack (show otherErr),
              suggestion = Nothing,
              details = []
            }

      adaptSessionError :: Hasql.SessionError -> Logic.Report
      adaptSessionError sessionErr = case sessionErr of
        Hasql.ScriptSessionError sql serverErr ->
          adaptServerError sql serverErr
        Hasql.StatementSessionError _ _ sql _ _ statementErr ->
          case statementErr of
            Hasql.ServerStatementError serverErr ->
              adaptServerError sql serverErr
            otherErr ->
              Logic.Report
                { path = [],
                  message = Text.pack (show otherErr),
                  suggestion = Nothing,
                  details = [("sql", sql)]
                }
        Hasql.ConnectionSessionError msg ->
          Logic.Report
            { path = [],
              message = msg,
              suggestion = Nothing,
              details = []
            }
        Hasql.MissingTypesSessionError types ->
          Logic.Report
            { path = [],
              message = "Missing database types",
              suggestion = Just "Ensure all referenced types exist in the database",
              details = [("types", Text.pack (show types))]
            }
        Hasql.DriverSessionError msg ->
          Logic.Report
            { path = [],
              message = msg,
              suggestion = Nothing,
              details = []
            }

      adaptServerError :: Text -> Hasql.ServerError -> Logic.Report
      adaptServerError sql (Hasql.ServerError errorCode message details hint position) =
        Logic.Report
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

instance Logic.DbOps (Fx Device Logic.Report) where
  executeMigration migrationText =
    HasqlDev.runSession (Hasql.Session.script migrationText)

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
      adaptAnalysisError :: Sessions.Error -> Logic.Report
      adaptAnalysisError err =
        Logic.Report
          { path = err.location,
            message = err.reason,
            suggestion = Nothing,
            details = err.details
          }

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

  getIndexes =
    HasqlDev.runSession GetIndexes.getIndexes
