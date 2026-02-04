{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module App
  ( main,
  )
where

import App.Algebras.CliUi qualified as Algebras.CliUi
import App.Commands qualified as Commands
import App.Services.Main qualified as Services.Main
import AppLogic qualified
import AppLogic.Migrations qualified
import Base.Prelude
import ParallelismAlgebra qualified
import StagingAlgebra qualified

main :: IO ()
main =
  Algebras.CliUi.main
    "pgn"
    "pGenie CLI"
    commands
    interpret

commands :: [Algebras.CliUi.Command App]
commands =
  [ Commands.generate
  ]

interpret :: App a -> IO a
interpret app =
  error "TODO"

-- | Main execution context.
newtype App a = App (Services.Main.Context -> IO (Either AppLogic.Error a))
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError AppLogic.Error,
      ParallelismAlgebra.Parallelism
    )
    via (ReaderT Services.Main.Context (ExceptT AppLogic.Error IO))

instance AppLogic.Effect App

instance AppLogic.Migrations.ControlsMigrations AppLogic.Error App

instance StagingAlgebra.Reports App
