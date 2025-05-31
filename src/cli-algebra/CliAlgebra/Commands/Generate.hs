-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module CliAlgebra.Commands.Generate (generate) where

import AppAlgebra qualified as AppAlgebra
import Base.Prelude
import CliAlgebra.Algebra
import GenAlgebra qualified as GenAlgebra
import Options.Applicative qualified as Opt

generate :: Command
generate =
  Command
    { name = "generate",
      description = "Generate code and missing signature files for the project",
      procedureArgParser
    }

procedureArgParser :: (AppAlgebra.Effect m) => Opt.Parser ([GenAlgebra.Gen] -> m ())
procedureArgParser = pure AppAlgebra.generate
