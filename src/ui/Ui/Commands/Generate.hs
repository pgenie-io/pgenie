-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module Ui.Commands.Generate (generate) where

import Base.Prelude
import Logic qualified
import Options.Applicative qualified as Opt
import Ui.Framework

generate :: (Logic.Caps m) => Command m
generate =
  Command
    { name = "generate",
      description = "Generate code and missing signature files for the project",
      parser,
      execute
    }

data Params = Params
  { strictSeqScans :: Bool
  }

parser :: Opt.Parser Params
parser =
  Params
    <$> Opt.switch
      ( Opt.long "strict-seq-scans"
          <> Opt.help "Fail the procedure if sequential scans are detected (instead of emitting warnings)"
      )

execute :: (Logic.Caps m) => Params -> m Text
execute params =
  Logic.generate
    Logic.GenerateOptions
      { strictSeqScans = params.strictSeqScans
      }
    $> ""
