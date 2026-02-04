module App.Services.Main.Context
  ( Context (..),
    Config (Config),
    Error (..),
    Event,
  )
where

import App.Algebras.Service
import App.Services.HasqlPool qualified as HasqlPool
import Base.Prelude
import Hasql.Connection qualified
import Hasql.Connection.Settings qualified
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Hasql.Session qualified

data Context = Context
  { pool :: HasqlPool.Context,
    path :: [Text],
    handler :: Event Context -> IO ()
  }

data instance Config Context = Config
  { pool :: Config HasqlPool.Context
  }
  deriving stock (Show, Eq)

newtype instance Error Context
  = PoolError (Error HasqlPool.Context)
  deriving newtype (Show, Eq)

data instance Event Context
  = StageEntered
      -- | Stage path.
      [Text]
  | StageExited
      -- | Stage path.
      [Text]
      -- | Duration in seconds.
      Double

instance IsService Context where
  start config _ _ = do
    error "TODO"

  stop _ = do
    error "TODO"
