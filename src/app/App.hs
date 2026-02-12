module App
  ( main,
  )
where

import App.Commands qualified as Commands
import App.Frameworks.CliUi qualified as Algebras.CliUi
import Base.Prelude
import Fx qualified
import Infra.Adapters.Main qualified as MainAdapter

main :: IO ()
main =
  Algebras.CliUi.main
    "pgn"
    "pGenie CLI"
    [ Commands.generate
    ]
    MainAdapter.run
