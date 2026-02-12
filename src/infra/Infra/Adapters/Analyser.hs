module Infra.Adapters.Analyser where

import Base.Prelude
import Fx
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Logic qualified
import TestcontainersFx.Scope qualified
import TestcontainersPostgresql qualified

newtype Device = Device Hasql.Pool.Pool

type Error = Logic.Error

adaptPoolUsageError :: Hasql.Pool.UsageError -> Error
adaptPoolUsageError =
  error "TODO"

adaptTestcontainersError :: SomeException -> Error
adaptTestcontainersError =
  error "TODO"

scope :: Fx.Scope env Error Device
scope = do
  (host, port) <-
    TestcontainersFx.Scope.testContainer
      ( TestcontainersPostgresql.setup
          TestcontainersPostgresql.Config
            { tagName = "postgres:18",
              auth = TestcontainersPostgresql.TrustAuth,
              forwardLogs = False
            }
      )
      & first adaptTestcontainersError
  pool <-
    acquire
      ( runTotalIO
          ( \_ ->
              Hasql.Pool.acquire
                ( Hasql.Pool.Config.settings
                    [ Hasql.Pool.Config.size 100
                    ]
                )
          )
      )

  error "TODO"

instance Logic.DbOps (Fx Device Error) where
  executeMigration = error "TODO"
  inferQueryTypes = error "TODO"
