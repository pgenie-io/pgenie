module App
  ( main,
    commands,
  )
where

import App.Commands qualified as Commands
import App.Frameworks.CommandCliApp
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
