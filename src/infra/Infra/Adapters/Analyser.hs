module Infra.Adapters.Analyser
  ( Device,
    scope,
  )
where

import Base.Prelude
import Base.Text qualified
import Data.Text qualified as Text
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
import Logic qualified
import TestcontainersPostgresql qualified

newtype Device = Device Hasql.Pool.Pool

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
          suggestion = hint,
          details =
            catMaybes
              [ Just ("code", errorCode),
                Just ("sql", maybe sql (\position -> Base.Text.pointToLocation sql position) position),
                ("details",) <$> details
              ]
        }

adaptTestcontainersError :: SomeException -> Logic.Error
adaptTestcontainersError err =
  Logic.Error
    { path = ["testcontainers"],
      message = Text.pack (displayException err),
      suggestion = Nothing,
      details = []
    }

scope :: (Logic.Event -> IO ()) -> Fx.Scope Logic.Error Device
scope emitEvent = do
  acquire $ runTotalIO \() -> emitEvent (Logic.StageEntered ["Starting Container"])
  (host, port) <-
    first
      adaptTestcontainersError
      ( Infra.Adapters.Analyser.Scopes.Testcontainers.testContainer
          ( TestcontainersPostgresql.setup
              TestcontainersPostgresql.Config
                { tagName = "postgres:18",
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
