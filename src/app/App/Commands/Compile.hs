module App.Commands.Compile (compile) where

import App.Algebras.CommandCliApp
import Base.Prelude
import Options.Applicative qualified as Opt

compile :: Command
compile =
  Command
    { name = "compile",
      description = "Compile the project",
      parser,
      execute
    }

data Params = Params

parser :: Opt.Parser Params
parser =
  pure Params

execute :: Params -> IO ()
execute =
  error "TODO: Implement compile command"
