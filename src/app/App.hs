module App
  ( main,
    commands,
  )
where

import App.Algebras.CommandCliApp
import App.Commands qualified as Commands
import Base.Prelude

main :: IO ()
main =
  runApp
    "pgn"
    "pGenie CLI"
    commands

commands :: [Command]
commands =
  [ Commands.compile,
    Commands.generateSignatures
  ]
