-- | Command that manages database indexes by generating migrations to add missing
-- indexes and remove redundant or excessive ones.
module Ui.Commands.ManageIndexes (manageIndexes) where

import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Procedures.ManageIndexes qualified as ManageIndexes
import Options.Applicative qualified as Opt
import Ui.Framework
import Utils.Prelude

manageIndexes :: forall m. (ManageIndexes.Port m) => Command m
manageIndexes =
  Command
    { name = "manage-indexes",
      description = "Output a migration to stdout that adds missing indexes and removes redundant or excessive ones",
      parser,
      execute
    }
  where
    parser :: Opt.Parser Params
    parser =
      Params
        <$> Opt.switch
          ( Opt.long "allow-redundant-indexes"
              <> Opt.help "Emit warnings about redundant indexes instead of removing them"
          )
        <*> Opt.switch
          ( Opt.long "add-migration"
              <> Opt.help "Also write the migration to a numbered file in migrations/ (fails if existing files do not follow the N.sql naming convention)"
          )

    execute :: ProjectFile.ProjectFile -> Params -> m Text
    execute projectFile params =
      ManageIndexes.run
        ManageIndexes.Params
          { projectFile,
            allowRedundantIndexes = params.allowRedundantIndexes,
            addMigration = params.addMigration
          }
        <&> (.migrationText)

-- | Parsed command-line options for the @manage-indexes@ command.
data Params = Params
  { allowRedundantIndexes :: Bool,
    addMigration :: Bool
  }
