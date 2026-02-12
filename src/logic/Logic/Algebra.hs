module Logic.Algebra where

import Base.Prelude hiding (readFile, writeFile)
import Data.Aeson qualified as Aeson
import Logic.Name qualified as Name
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import ParallelismAlgebra

-- * Error

-- | Error report.
data Error = Error
  { path :: [Text],
    message :: Text,
    suggestion :: Maybe Text,
    details :: [(Text, Text)]
  }

-- * States

data ProjectFileLoaded = ProjectFileLoaded
  { configFilePath :: Path,
    owner :: Name.Name,
    name :: Name.Name,
    version :: Gen.Input.Version,
    -- | Path to the directory with migrations.
    migrationsDir :: Path,
    -- | Path to the directory with queries.
    queriesDir :: Path,
    -- | List of codegen configurations.
    artifacts :: [Artifact]
  }

data Artifact = Artifact
  { name :: Text,
    genUrl :: Gen.Location,
    config :: Aeson.Value
  }

data QueryListed = QueryListed
  { name :: Name.Name,
    filePath :: Path,
    signatureFilePath :: Maybe Path
  }

data InferredQueryTypes = InferredQueryTypes
  { params :: [Gen.Input.Member],
    resultColumns :: [Gen.Input.Member],
    mentionedCustomTypes :: [Gen.Input.CustomType]
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

-- | Capability for reporting progress of stages and substages.
class (Monad m) => Reports m where
  enterStage :: [Text] -> m ()
  exitStage :: [Text] -> Double -> m ()

class (MonadError Error m) => DbOps m where
  executeMigration :: Text -> m ()
  inferQueryTypes :: Text -> m InferredQueryTypes

class (MonadError Error m) => FsOps m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()
  listDir :: Path -> m [Path]

-- | Domain operations.
class (MonadError Error m) => LoadsGen m where
  loadGen :: Gen.Location -> m Gen.Gen

-- | Combined capabilities required by the logic.
type Caps m =
  ( LoadsGen m,
    DbOps m,
    Parallelism m,
    FsOps m,
    Reports m
  )
