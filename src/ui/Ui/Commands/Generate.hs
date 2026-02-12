-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module Ui.Commands.Generate (generate) where

import Base.Prelude
import Logic qualified
import Options.Applicative qualified as Opt
import Ui.Frameworks.CliUi

generate :: (Logic.Caps m) => Command m
generate =
  Command
    { name = "generate",
      description = "Generate code and missing signature files for the project",
      parser,
      execute
    }

data Params = Params

parser :: Opt.Parser Params
parser =
  pure Params

execute :: (Logic.Caps m) => Params -> m ()
execute _params = Logic.generate
