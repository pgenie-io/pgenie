-- |
-- Administrative connection to the DB.
-- Manages the databases.
module App.Services.DbAdmin.Context
  ( Context (..),
    Config (Config),
    Error (..),
    Event,
  )
where

import App.Algebras.Service
import App.Services.PqConnection qualified as PqConnection
import Base.Prelude
import Hasql.Connection qualified
import Hasql.Connection.Settings qualified
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Hasql.Session qualified

data Context = Context
  { pool :: Hasql.Pool.Pool,
    counter :: IORef Word16
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
    ref <- newIORef 0
    pure (Started (Context pool ref))
    where
      compilePoolSettings :: Config Context -> Hasql.Pool.Config.Config
      compilePoolSettings config =
        Hasql.Pool.Config.settings
          [ Hasql.Pool.Config.size config.poolSize,
            Hasql.Pool.Config.staticConnectionSettings
              (compileConnectionSettings config)
          ]

      compileConnectionSettings :: Config Context -> Hasql.Connection.Settings.Settings
      compileConnectionSettings config =
        mconcat
          [ Hasql.Connection.Settings.host config.host,
            Hasql.Connection.Settings.hostAndPort config.host config.port,
            Hasql.Connection.Settings.user config.user,
            Hasql.Connection.Settings.password config.password,
            Hasql.Connection.Settings.dbname config.database
          ]

  stop (Context pool _) = do
    Hasql.Pool.release pool

instance ContainsService PqConnection.Context Context where
  embedProcedure subprocedure service _notify =
    Hasql.Pool.use service.pool session
      & fmap processResult
    where
      session =
        Hasql.Session.onLibpqConnection \pqConnection -> do
          result <- subprocedure (to pqConnection) subnotify
          pure (Right result, pqConnection)
      subnotify _pqConnectionEvent =
        pure ()
      processResult = \case
        Left usageError -> Left (Error usageError)
        Right (Left _pqError) -> error "TODO"
        Right (Right result) -> Right result
