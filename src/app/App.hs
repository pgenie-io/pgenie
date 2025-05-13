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
  [ modelCommand (Proxy @Commands.Compile),
    modelCommand (Proxy @Commands.GenerateSignatures)
  ]
