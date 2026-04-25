module Logic.Algebra
  ( module Logic.Algebra,
    Report (..),
  )
where

import Logic.Report (Report (..))
import PGenieGen qualified as Gen
import Utils.Prelude hiding (readFile, writeFile)

-- * Analyse options

data AnalyseOptions = AnalyseOptions
  { failOnSeqScans :: Bool
  }
  deriving stock (Eq, Show)

-- * Generate options

data GenerateOptions = GenerateOptions
  { failOnSeqScans :: Bool
  }
  deriving stock (Eq, Show)

-- * Manage-indexes options

data ManageIndexesOptions = ManageIndexesOptions
  { allowRedundantIndexes :: Bool,
    -- | When set, write the generated migration to a numbered file in the
    -- @migrations/@ directory in addition to printing it to stdout.
    addMigration :: Bool
  }
  deriving stock (Eq, Show)

-- * Model format

data ModelFormat = ModelFormatJson | ModelFormatDhall
  deriving stock (Eq, Show)

-- * Cross-cutting capability classes

-- |
-- - Reports progress.
-- - Reports stage enter and exit for logging.
-- - Reports parallelism as @enters - exits@. Amount of actively running stages.
class (Monad m) => Stages m where
  -- | Wrap an action as a stage in progress.
  stage ::
    -- | Name of the stage. May be empty.
    Text ->
    -- | Amount of substages.
    --
    -- Each nested stage exit will increase the progress within this stage by @1 / amountOfSubstages@.
    --
    -- If there's no substages, pass @0@. Then only the exit of the whole stage will increase the progress.
    Int ->
    m a ->
    m a

-- | Emission of non-fatal problem reports.
class (Monad m) => Warns m where
  warn :: Report -> m ()

class (MonadError Report m) => FsOps m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()
  listDir :: Path -> m [Path]

-- | Domain operations.
class (MonadError Report m) => LoadsGen m where
  loadGen ::
    Gen.Location ->
    -- | Possible integrity hash for caching.
    Maybe Text ->
    -- | Action producing the gen along with its integrity hash.
    m (Gen.Gen, Text)
