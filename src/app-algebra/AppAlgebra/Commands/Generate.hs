-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module AppAlgebra.Commands.Generate (generate) where

import AppAlgebra.Algebra qualified as AppAlgebra
import AppAlgebra.App
import Base.Prelude
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
