-- | Port for executing migration SQL against the target database.
module Logic.Migrations
  ( ExecutesMigrations (..),
  )
where

import Logic.Report (Report (..))
import Utils.Prelude

-- | Port for executing a migration SQL script against the database.
class (MonadError Report m) => ExecutesMigrations m where
  executeMigration :: Text -> m ()
