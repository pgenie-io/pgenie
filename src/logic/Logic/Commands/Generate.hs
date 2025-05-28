-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module Logic.Commands.Generate (generate) where

import Base.Prelude
import GenAlgebra qualified as GenAlgebra
import Logic.Algebra qualified as AppAlgebra
import Logic.App
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
