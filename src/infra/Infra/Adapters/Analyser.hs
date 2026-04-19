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

adaptPoolUsageError :: Hasql.Pool.UsageError -> Logic.Error
adaptPoolUsageError err = case err of
  Hasql.Pool.SessionUsageError sessionErr ->
    adaptSessionError sessionErr
  otherErr ->
    Logic.Error
      { path = [],
        message = Text.pack (show otherErr),
        suggestion = Nothing,
        details = []
      }
  where
    adaptSessionError :: Hasql.SessionError -> Logic.Error
    adaptSessionError sessionErr = case sessionErr of
      Hasql.ScriptSessionError sql serverErr ->
        adaptServerError sql serverErr
      Hasql.StatementSessionError _ _ sql _ _ statementErr ->
        case statementErr of
          Hasql.ServerStatementError serverErr ->
            adaptServerError sql serverErr
          otherErr ->
            Logic.Error
              { path = [],
                message = Text.pack (show otherErr),
                suggestion = Nothing,
                details = [("sql", sql)]
              }
      Hasql.ConnectionSessionError msg ->
        Logic.Error
          { path = [],
            message = msg,
            suggestion = Nothing,
            details = []
          }
      Hasql.MissingTypesSessionError types ->
        Logic.Error
          { path = [],
            message = "Missing database types",
            suggestion = Just "Ensure all referenced types exist in the database",
            details = [("types", Text.pack (show types))]
          }
      Hasql.DriverSessionError msg ->
        Logic.Error
          { path = [],
            message = msg,
            suggestion = Nothing,
            details = []
          }

    adaptServerError :: Text -> Hasql.ServerError -> Logic.Error
    adaptServerError sql (Hasql.ServerError errorCode message details hint position) =
      Logic.Error
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

adaptTestcontainersError :: SomeException -> Logic.Error
adaptTestcontainersError err =
  Logic.Error
    { path = ["testcontainers"],
      message = Text.pack (displayException err),
      suggestion = Nothing,
      details = []
    }

scope :: Source -> (Logic.Event -> IO ()) -> Fx.Scope Logic.Error Device
scope source emitEvent = case source of
  DockerSource {postgresTag} -> scopeViaDocker postgresTag emitEvent
  RunningServerSource {connectionUrl, targetMajorVersion} ->
    scopeViaRunningServer connectionUrl targetMajorVersion emitEvent

scopeViaDocker :: Text -> (Logic.Event -> IO ()) -> Fx.Scope Logic.Error Device
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
    acquire
      ( runTotalIO
          ( \() ->
              Hasql.Pool.acquire
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
          )
      )

  acquire $ runTotalIO \() -> emitEvent (Logic.StageExited ["Connecting"] 0.1)

  registerRelease
    ( runTotalIO
        ( \() ->
            Hasql.Pool.release pool
        )
    )

  pure (Device pool)

scopeViaRunningServer :: Text -> Int -> (Logic.Event -> IO ()) -> Fx.Scope Logic.Error Device
scopeViaRunningServer connectionUrl targetMajorVersion emitEvent = do
  let connStrBS = TextEncoding.encodeUtf8 connectionUrl

  acquire $ runTotalIO \() -> emitEvent (Logic.StageEntered ["Connecting"])

  -- Validate that the running server's major version matches the project target.
  serverMajorVersion <-
    acquire $
      runPartialIO \() ->
        fmap (first adaptLibpqError) (queryServerMajorVersion connStrBS)

  when (serverMajorVersion /= targetMajorVersion) do
    throwError
      Logic.Error
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
                  <> "' in project1.pgn.yaml to match the running server, "
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
  tempDbName <- acquire $ runTotalIO \() -> generateTempDbName

  -- Create the temporary database on the admin connection.
  acquire $
    runPartialIO \() ->
      fmap (first adaptAdminError) $
        execAdminDDL connStrBS ("CREATE DATABASE " <> quoteIdent tempDbName)

  -- Open the analysis pool on the temporary database.
  let analysisSettings =
        Hasql.Connection.Settings.connectionString connectionUrl
          <> Hasql.Connection.Settings.dbname tempDbName
  pool <-
    acquire $
      runTotalIO \() ->
        Hasql.Pool.acquire
          ( Hasql.Pool.Config.settings
              [ Hasql.Pool.Config.size 100,
                Hasql.Pool.Config.staticConnectionSettings analysisSettings
              ]
          )

  -- Release the pool and drop the temporary database on cleanup.
  registerRelease $
    runTotalIO \() -> do
      Hasql.Pool.release pool
      -- Ignore errors during cleanup so the process exits cleanly.
      _ <- execAdminDDL connStrBS ("DROP DATABASE " <> quoteIdent tempDbName)
      pure ()

  acquire $ runTotalIO \() -> emitEvent (Logic.StageExited ["Connecting"] 0.1)

  pure (Device pool)

-- | Query the server major version via a direct libpq connection.
-- Returns @server_version_num / 10000@, e.g. @18@ for PostgreSQL 18.x.
queryServerMajorVersion :: ByteString -> IO (Either Text Int)
queryServerMajorVersion connStr = do
  conn <- Pq.connectdb connStr
  st <- Pq.status conn
  result <-
    if st /= Pq.ConnectionOk
      then fmap (Left . connErrMsg) (Pq.errorMessage conn)
      else do
        mResult <- Pq.exec conn "SELECT current_setting('server_version_num')::int / 10000"
        case mResult of
          Nothing -> fmap (Left . connErrMsg) (Pq.errorMessage conn)
          Just res -> do
            rst <- Pq.resultStatus res
            if rst == Pq.TuplesOk
              then do
                mVal <- Pq.getvalue res (Pq.Row 0) (Pq.Col 0)
                pure $ case mVal >>= readMaybe . Text.unpack . TextEncoding.decodeUtf8Lenient of
                  Just n -> Right n
                  Nothing -> Left "Could not parse server_version_num"
              else fmap (Left . resultErrMsg) (Pq.resultErrorMessage res)
  Pq.finish conn
  pure result

-- | Execute a DDL statement via a direct libpq connection (outside a transaction).
execAdminDDL :: ByteString -> ByteString -> IO (Either Text ())
execAdminDDL connStr sql = do
  conn <- Pq.connectdb connStr
  st <- Pq.status conn
  result <-
    if st /= Pq.ConnectionOk
      then fmap (Left . connErrMsg) (Pq.errorMessage conn)
      else do
        mResult <- Pq.exec conn sql
        case mResult of
          Nothing -> fmap (Left . connErrMsg) (Pq.errorMessage conn)
          Just res -> do
            rst <- Pq.resultStatus res
            if rst `elem` [Pq.CommandOk, Pq.TuplesOk]
              then pure (Right ())
              else fmap (Left . resultErrMsg) (Pq.resultErrorMessage res)
  Pq.finish conn
  pure result

-- | Double-quote a PostgreSQL identifier.
quoteIdent :: Text -> ByteString
quoteIdent ident = "\"" <> TextEncoding.encodeUtf8 (Text.replace "\"" "\"\"" ident) <> "\""

connErrMsg :: Maybe ByteString -> Text
connErrMsg = maybe "Connection error" TextEncoding.decodeUtf8Lenient

resultErrMsg :: Maybe ByteString -> Text
resultErrMsg = maybe "Execution error" TextEncoding.decodeUtf8Lenient

adaptLibpqError :: Text -> Logic.Error
adaptLibpqError msg =
  Logic.Error
    { path = [],
      message = "Failed to connect to database server: " <> msg,
      suggestion = Nothing,
      details = []
    }

adaptAdminError :: Text -> Logic.Error
adaptAdminError msg =
  Logic.Error
    { path = [],
      message = "Failed to create temporary analysis database",
      suggestion =
        Just "Ensure the database user has the CREATEDB privilege",
      details = [("reason", msg)]
    }

generateTempDbName :: IO Text
generateTempDbName = do
  uuid <- UUID.V4.nextRandom
  let uuidText = Text.pack (UUID.toString uuid)
  pure ("pgenie_" <> Text.map (\c -> if c == '-' then '_' else c) uuidText)

instance HasqlDev.RunsSession (Fx Device Logic.Error) where
  runSession session =
    runPartialIO \(Device pool) ->
      first adaptPoolUsageError <$> Hasql.Pool.use pool session

instance Logic.DbOps (Fx Device Logic.Error) where
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
      adaptAnalysisError :: Sessions.Error -> Logic.Error
      adaptAnalysisError err =
        Logic.Error
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
