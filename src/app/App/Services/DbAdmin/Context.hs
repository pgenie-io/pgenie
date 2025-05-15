{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

-- |
-- Administrative connection to the DB.
-- Manages the databases.
--
-- Please notice that operations in this service are not safe against injection attacks.
-- They are only intended to be used internally and must not depend on user-supplied input.
module App.Services.DbAdmin.Context
  ( Context (..),
    Config (..),
    Error,
    Event,
  )
where

import App.Frameworks.Service
import Base.Prelude
import Data.Text qualified as Text
import Data.UUID qualified as Uuid
import Data.UUID.V4 qualified as Uuid
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Connection.Setting.Connection.Param qualified
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Hasql.Session qualified
import Hasql.Statement qualified

data Context = Context Hasql.Pool.Pool (IORef Word16)

data instance Config Context = Config
  { poolSize :: Int,
    host :: Text,
    port :: Word16,
    user :: Text,
    password :: Text,
    database :: Text
  }
  deriving stock (Show, Eq)

data instance Error Context

data instance Event Context

instance IsService Context where
  start config _ = do
    pool <- Hasql.Pool.acquire (compilePoolSettings config)
    ref <- newIORef 0
    pure (Started (Context pool ref))
  stop (Context pool _) = do
    Hasql.Pool.release pool

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
