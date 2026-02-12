module Main (main) where

import Base.Prelude
import Infra.Adapters.Main qualified as MainAdapter
import Ui.Commands qualified as Commands
import Ui.Frameworks.CliUi qualified as Algebras.CliUi

main :: IO ()
main =
  Algebras.CliUi.main
    "pgn"
    "pGenie CLI"
    [ Commands.generate
    ]
    MainAdapter.run
