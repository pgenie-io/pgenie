{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module App.Runtimes.Main where

import App.Frameworks.CliUi qualified as Algebras.CliUi
import App.Services.Main qualified as Services.Main
import AppLogic qualified
import Base.Prelude
import ParallelismAlgebra qualified
import StagingAlgebra qualified

run :: Main a -> IO a
run =
  error "TODO"

-- | Main execution context.
newtype Main a = Main (Services.Main.Context -> IO (Either AppLogic.Error a))
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError AppLogic.Error,
      ParallelismAlgebra.Parallelism
    )
    via (ReaderT Services.Main.Context (ExceptT AppLogic.Error IO))

instance AppLogic.DbOps Main

instance AppLogic.DomainOps Main

instance AppLogic.ControlsMigrations AppLogic.Error Main

instance StagingAlgebra.Stages Main
