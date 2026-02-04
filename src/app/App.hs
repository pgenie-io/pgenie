{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module App
  ( main,
  )
where

import App.Algebras.CliUi qualified as Algebras.CliUi
import App.Commands qualified as Commands
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

newtype App a = App (())

instance Functor App

instance Applicative App

instance Monad App

instance MonadError AppLogic.Error App

instance ParallelismAlgebra.Parallelism App

instance AppLogic.Effect App

instance AppLogic.Migrations.ControlsMigrations AppLogic.Error App

instance StagingAlgebra.Reports App
