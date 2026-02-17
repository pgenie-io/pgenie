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

-- * Capabilities

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
