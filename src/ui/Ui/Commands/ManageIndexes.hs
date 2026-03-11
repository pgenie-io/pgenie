-- | Command that manages database indexes by generating migrations to add missing
-- indexes and remove redundant or excessive ones.
module Ui.Commands.ManageIndexes (manageIndexes) where

import Base.Prelude
import Logic qualified
import Options.Applicative qualified as Opt
import Ui.Framework

manageIndexes :: (Logic.Caps m) => Command m
manageIndexes =
  Command
    { name = "manage-indexes",
      description = "Generate a migration to add missing indexes and remove redundant or excessive ones",
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
          <> Opt.help "Automatically generate a migration file with index changes"
      )
    <*> Opt.switch
      ( Opt.long "allow-redundant-indexes"
          <> Opt.help "Downgrade redundant index issues to warnings instead of errors"
      )

execute :: (Logic.Caps m) => Params -> m Text
execute params =
  Logic.manageIndexes
    Logic.ManageIndexesOptions
      { fix = params.fix,
        allowRedundantIndexes = params.allowRedundantIndexes
      }
    $> ""
