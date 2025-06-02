-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module CliUi.Commands.Generate (generate) where

import AppAlgebra qualified
import AppLogic qualified
import Base.Prelude
import CommandCliUiAlgebra
import Options.Applicative qualified as Opt

generate :: (AppAlgebra.Effect m) => Command m
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

execute :: (AppAlgebra.Effect m) => Params -> m ()
execute _params = AppLogic.generate
