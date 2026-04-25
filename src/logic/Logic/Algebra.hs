module Logic.Algebra
  ( module Logic.Algebra,
    Report (..),
  )
where

import Logic.ProjectFile (ProjectFile)
import Logic.Report (Report (..))
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import Utils.Prelude hiding (readFile, writeFile)

-- * States

data InferredQueryTypes = InferredQueryTypes
  { params :: [InferredParam],
    resultColumns :: [Gen.Input.Member],
    mentionedCustomTypes :: [Gen.Input.CustomType]
  }
  deriving stock (Eq, Show)

data InferredParam = InferredParam
  { isNullable :: Bool,
    type_ :: Gen.Input.Value
  }
  deriving stock (Eq, Show)

-- * Seq-scan detection

-- | A finding of sequential scan in a query execution plan.
data SeqScanFinding = SeqScanFinding
  { -- | Name of the table being sequentially scanned.
    tableName :: Text,
    -- | The filter condition from the EXPLAIN output.
    filterCondition :: Text,
    -- | Suggested columns to create an index on.
    suggestedIndexColumns :: [Text]
  }
  deriving stock (Eq, Show)

-- * Index info

data IndexInfo = IndexInfo
  { indexName :: Text,
    tableName :: Text,
    schemaName :: Text,
    columns :: [Text],
    isUnique :: Bool,
    isPrimary :: Bool,
    indexMethod :: Text,
    predicate :: Maybe Text
  }
  deriving stock (Eq, Show)

-- * Index optimization results

-- | An action recommended by the index optimizer.
data IndexAction
  = -- | Drop an index that is unnecessary.
    DropIndex IndexInfo DropReason
  | -- | Create a new index to cover a missing access pattern.
    CreateIndex
      { tableName :: Text,
        columns :: [Text]
      }
  deriving stock (Eq, Show)

-- | Reason why an index should be dropped.
data DropReason
  = -- | This index's columns are a leading prefix of the superseding index's columns.
    PrefixRedundancy IndexInfo
  | -- | This index is an exact duplicate of another index.
    ExactDuplicate IndexInfo
  | -- | This composite index has trailing columns that are not needed by any query.
    --   The replacement columns are provided.
    ExcessiveComposite [Text]
  | -- | This index is not used by any observed query need on the same table.
    UnusedByQueries
  deriving stock (Eq, Show)

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

-- * Capabilities

-- | Typeclasses representing capabilities required by the logic and serving as ports as per the hexagonal architecture.
--
-- They allow to implement the overall orchestration logic in a way that is decoupled from specific implementations of these capabilities, making it easier to test and maintain.
-- We simply state what we need for the logic to work and provide an interface for the implementations to conform to.

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

class (MonadError Report m) => DbOps m where
  executeMigration :: Text -> m ()
  inferQueryTypes :: Text -> m (InferredQueryTypes, [Report])
  explainQuery :: Text -> m [Text]
  getIndexes :: m [IndexInfo]

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

class (Monad m) => LoadsProjectFile m where
  loadProjectFile :: m ProjectFile

-- | Combined capabilities required by the logic.
type Caps m =
  ( MonadParallel m,
    LoadsProjectFile m,
    LoadsGen m,
    DbOps m,
    FsOps m,
    Stages m,
    Warns m
  )
