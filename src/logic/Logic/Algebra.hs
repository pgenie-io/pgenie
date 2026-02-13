module Logic.Algebra where

import Base.Prelude hiding (readFile, writeFile)
import Logic.Name qualified as Name
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output

-- * Error

-- | Error report.
data Error = Error
  { path :: [Text],
    message :: Text,
    suggestion :: Maybe Text,
    details :: [(Text, Text)]
  }

-- * Event

data Event
  = StageEntered [Text]
  | StageExited [Text] Double
  | WarningEmitted Error
  | Failed Error

-- * States

data QueryListed = QueryListed
  { name :: Name.Name,
    filePath :: Path,
    signatureFilePath :: Maybe Path
  }

data InferredQueryTypes = InferredQueryTypes
  { params :: [InferredParam],
    resultColumns :: [Gen.Input.Member],
    mentionedCustomTypes :: [Gen.Input.CustomType]
  }

data InferredParam = InferredParam
  { isNullable :: Bool,
    type_ :: Gen.Input.Value
  }

data GeneratedArtifact = GeneratedArtifact
  { name :: Text,
    warnings :: [Gen.Output.Report],
    filePaths :: [Path]
  }

data SignatureGenerated = SignatureGenerated
  { filePath :: Path,
    replaced :: Bool
  }

data QuerySignature
  = QuerySignature
      -- | Parameters of the query.
      [Gen.Input.Member]
      -- | Result of the query.
      (Maybe Gen.Input.ResultRows)

data QueriesMetadataMerged = QueriesMetadataMerged
  { queries :: [Gen.Input.Query],
    customTypes :: [Gen.Input.CustomType]
  }

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

class (Monad m) => Emits m where
  emit :: Event -> m ()

class (MonadError Error m) => DbOps m where
  executeMigration :: Text -> m ()
  inferQueryTypes :: Text -> m (InferredQueryTypes, [Error])

class (MonadError Error m) => FsOps m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()
  listDir :: Path -> m [Path]

-- | Domain operations.
class (MonadError Error m) => LoadsGen m where
  loadGen :: Gen.Location -> m Gen.Gen

-- | Combined capabilities required by the logic.
type Caps m =
  ( MonadParallel m,
    LoadsGen m,
    DbOps m,
    FsOps m,
    Emits m
  )
