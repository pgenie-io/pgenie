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
  { fix :: Bool,
    allowRedundantIndexes :: Bool
  }

parser :: Opt.Parser Params
parser =
  Params
    <$> Opt.switch
      ( Opt.long "fix"
          <> Opt.help "Automatically generate a fixing migration"
      )
    <*> Opt.switch
      ( Opt.long "allow-redundant-indexes"
          <> Opt.help "Downgrade redundant index errors to warnings"
      )

execute :: (Logic.Caps m) => Params -> m ()
execute params =
  Logic.generate
    Logic.GenerateOptions
      { fix = params.fix,
        allowRedundantIndexes = params.allowRedundantIndexes
      }
