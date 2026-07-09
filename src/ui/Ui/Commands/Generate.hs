-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module Ui.Commands.Generate (generate) where

import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Procedures.Generate qualified as Generate
import Options.Applicative qualified as Opt
import Ui.Framework
import Utils.Prelude

generate :: forall m. (Generate.Port m) => Command m
generate =
  Command
    { name = "generate",
      description = "Generate code and missing signature files for the project",
      parser,
      execute
    }
  where
    parser :: Opt.Parser Params
    parser =
      Params
        <$> Opt.switch
          ( Opt.long "fail-on-seq-scans"
              <> Opt.help "Fail the procedure if sequential scans are detected (instead of emitting warnings)"
          )

    execute :: ProjectFile.ProjectFile -> Params -> m Text
    execute projectFile params =
      Generate.run
        Generate.Params
          { projectFile,
            failOnSeqScans = params.failOnSeqScans
          }
        $> ""

-- | Parsed command-line options for the @generate@ command.
data Params = Params
  { failOnSeqScans :: Bool
  }
