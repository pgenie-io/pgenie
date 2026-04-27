-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module Ui.Commands.Generate (generate) where

import Logic.Features.Generate.Workflows.Generate qualified as Generate
import Logic.Features.ProjectModel.Types.ProjectModel qualified as ProjectFile
import Options.Applicative qualified as Opt
import Ui.Framework
import Utils.Prelude

generate :: (Generate.Port m) => Command m
generate =
  Command
    { name = "generate",
      description = "Generate code and missing signature files for the project",
      parser,
      execute
    }

data Params = Params
  { failOnSeqScans :: Bool
  }

parser :: Opt.Parser Params
parser =
  Params
    <$> Opt.switch
      ( Opt.long "fail-on-seq-scans"
          <> Opt.help "Fail the procedure if sequential scans are detected (instead of emitting warnings)"
      )

execute :: (Generate.Port m) => ProjectFile.ProjectFile -> Params -> m Text
execute projectFile params =
  Generate.run
    Generate.Params
      { projectFile,
        failOnSeqScans = params.failOnSeqScans
      }
    $> ""
