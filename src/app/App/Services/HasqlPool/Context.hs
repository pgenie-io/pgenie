module App.Services.HasqlPool.Context
  ( Context (..),
    Config (Config),
    Error (..),
    Event,
  )
where

import App.Frameworks.Service
import App.Services.PqConnection qualified as PqConnection
import Base.Prelude
import Hasql.Connection qualified
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Connection.Setting.Connection.Param qualified
import Hasql.Pool qualified
import Hasql.Pool.Config qualified

data Context = Context
  { pool :: Hasql.Pool.Pool
  }

data instance Config Context = Config
  { poolSize :: Int,
    host :: Text,
    port :: Word16,
    user :: Text,
    password :: Text,
    database :: Text
  }
  deriving stock (Show, Eq)

newtype instance Error Context = Error
  { base :: Hasql.Pool.UsageError
  }
  deriving newtype (Show, Eq)

data instance Event Context

instance IsService Context where
  start config _ _ = do
    pool <- Hasql.Pool.acquire (compilePoolSettings config)
    pure (Started (Context pool))
    where
      compilePoolSettings :: Config Context -> Hasql.Pool.Config.Config
      compilePoolSettings config =
        Hasql.Pool.Config.settings
          [ Hasql.Pool.Config.size config.poolSize,
            Hasql.Pool.Config.staticConnectionSettings
              (compileConnectionSettings config)
          ]

      compileConnectionSettings :: Config Context -> [Hasql.Connection.Setting.Setting]
      compileConnectionSettings config =
        [ Hasql.Connection.Setting.connection
            ( Hasql.Connection.Setting.Connection.params
                [ Hasql.Connection.Setting.Connection.Param.host config.host,
                  Hasql.Connection.Setting.Connection.Param.port config.port,
                  Hasql.Connection.Setting.Connection.Param.user config.user,
                  Hasql.Connection.Setting.Connection.Param.password config.password,
                  Hasql.Connection.Setting.Connection.Param.dbname config.database
                ]
            )
        ]

  stop (Context pool) = do
    Hasql.Pool.release pool

instance ContainsService PqConnection.Context Context where
  embedProcedure subprocedure service _notify =
    Hasql.Pool.use service.pool session
      & fmap processResult
    where
      session = do
        connection <- ask
        liftIO do
          Hasql.Connection.withLibPQConnection connection \pqConnection ->
            subprocedure (to pqConnection) subnotify
      subnotify _pqConnectionEvent =
        pure ()
      processResult = \case
        Left usageError -> Left (Error usageError)
        Right (Left _pqError) -> error "TODO"
        Right (Right result) -> Right result
