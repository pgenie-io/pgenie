module App
  ( main,
  )
where

import App.Algebras.CliUi qualified as Algebras.CliUi
import App.Commands qualified as Commands
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
