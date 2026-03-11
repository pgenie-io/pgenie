module Logic.Algebra where

import Base.Prelude hiding (readFile, writeFile)
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input

-- * Error

-- | Error report.
data Error = Error
  { path :: [Text],
    message :: Text,
    suggestion :: Maybe Text,
    details :: [(Text, Text)]
  }
  deriving stock (Eq, Show)

-- * Event

data Event
  = StageEntered [Text]
  | StageExited [Text] Double
  | WarningEmitted Error
  | Failed Error
  deriving stock (Eq, Show)

-- * States

data InferredQueryTypes = InferredQueryTypes
  { params :: [InferredParam],
    resultColumns :: [Gen.Input.Member],
    mentionedCustomTypes :: [Gen.Input.CustomType]
  }

data InferredParam = InferredParam
  { isNullable :: Bool,
    type_ :: Gen.Input.Value
  }

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
    writeToFile :: Bool
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
class (Monad m) => Emits m where
  emit :: Event -> m ()

class (MonadError Error m) => DbOps m where
  executeMigration :: Text -> m ()
  inferQueryTypes :: Text -> m (InferredQueryTypes, [Error])
  explainQuery :: Text -> m [Text]
  getIndexes :: m [IndexInfo]

class (MonadError Error m) => FsOps m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()
  listDir :: Path -> m [Path]

-- | Domain operations.
class (MonadError Error m) => LoadsGen m where
  loadGen ::
    Gen.Location ->
    -- | Possible integrity hash for caching.
    Maybe Text ->
    -- | Action producing the gen along with its integrity hash.
    m (Gen.Gen, Text)

-- | Combined capabilities required by the logic.
type Caps m =
  ( MonadParallel m,
    LoadsGen m,
    DbOps m,
    FsOps m,
    Emits m
  )
