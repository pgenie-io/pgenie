{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module App.Runtimes.Main where

import App.Frameworks.CliUi qualified as Algebras.CliUi
import App.Services.Main qualified as Services.Main
import Base.Prelude
import Logic qualified
import ParallelismAlgebra qualified
import StagingAlgebra qualified

run :: Main a -> IO a
run =
  error "TODO"

-- | Main execution context.
newtype Main a = Main (Services.Main.Context -> IO (Either Logic.Error a))
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError Logic.Error,
      ParallelismAlgebra.Parallelism
    )
    via (ReaderT Services.Main.Context (ExceptT Logic.Error IO))

instance Logic.DbOps Main

instance Logic.FsOps Main

instance Logic.LoadsGen Main

instance StagingAlgebra.Stages Main

instance Logic.Reports Main where
  enterStage _ = pure ()
  exitStage _ _ = pure ()
