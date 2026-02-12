module Infra.Adapters.Analyser
  ( Device,
    scope,
  )
where

import Base.Prelude
import Data.Text qualified as Text
import Fx
import Hasql.Connection.Settings qualified
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
adaptPoolUsageError err =
  Logic.Error
    { path = ["db"],
      message = Text.pack (show err),
      suggestion = Nothing,
      details = []
    }

adaptTestcontainersError :: SomeException -> Logic.Error
adaptTestcontainersError err =
  Logic.Error
    { path = ["testcontainers"],
      message = Text.pack (displayException err),
      suggestion = Nothing,
      details = []
    }

scope :: Fx.Scope Logic.Error Device
scope = do
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

  pool <-
    acquire
      ( runTotalIO
          ( \() ->
              Hasql.Pool.acquire
                ( Hasql.Pool.Config.settings
                    [ Hasql.Pool.Config.size 100,
                      Hasql.Pool.Config.staticConnectionSettings
                        (Hasql.Connection.Settings.hostAndPort host port)
                    ]
                )
          )
      )

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
            details = []
          }
