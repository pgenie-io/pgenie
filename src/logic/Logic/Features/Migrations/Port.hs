-- | Port for executing migration SQL against the target database.
module Logic.Features.Migrations.Port
  ( ExecutesMigrations (..),
  )
where

import Logic.Features.Reporting.Types.Report (Report (..))
import Utils.Prelude

-- | Port for executing a migration SQL script against the database.
class (MonadError Report m) => ExecutesMigrations m where
  executeMigration :: Text -> m ()
