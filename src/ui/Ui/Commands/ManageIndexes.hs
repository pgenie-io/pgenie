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
  { allowRedundantIndexes :: Bool
  }

parser :: Opt.Parser Params
parser =
  Params
    <$> Opt.switch
      ( Opt.long "allow-redundant-indexes"
          <> Opt.help "Emit warnings about redundant indexes instead of removing them"
      )

execute :: (Logic.Caps m) => Params -> m Text
execute params =
  Logic.manageIndexes
    Logic.ManageIndexesOptions
      { allowRedundantIndexes = params.allowRedundantIndexes
      }
    $> ""
