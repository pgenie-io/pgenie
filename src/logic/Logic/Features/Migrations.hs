-- | Port for executing migration SQL against the target database.
module Logic.Features.Migrations
  ( ExecutesMigrations (..),
  )
where

import Logic.Features.Report (Report (..))
import Utils.Prelude

-- | Port for executing a migration SQL script against the database.
class (MonadError Report m) => ExecutesMigrations m where
  executeMigration :: Text -> m ()
