-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module AppCommands.Generate (generate) where

import AppLogic qualified
import Base.Prelude
import CommandCliUiAlgebra
import Options.Applicative qualified as Opt

generate :: (AppLogic.Effect m) => Command m
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

execute :: (AppLogic.Effect m) => Params -> m ()
execute _params = AppLogic.generate
