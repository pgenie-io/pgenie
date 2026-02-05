module App
  ( main,
  )
where

import App.Commands qualified as Commands
import App.Frameworks.CliUi qualified as Algebras.CliUi
import App.Runtimes.Main qualified as Effects.Main
import Base.Prelude

main :: IO ()
main =
  Algebras.CliUi.main
    "pgn"
    "pGenie CLI"
    [ Commands.generate
    ]
    Effects.Main.run
