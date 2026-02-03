module AppAlgebra where

import AppAlgebra.Migrations qualified as Migrations
import Base.Prelude hiding (writeFile)
import Data.Aeson qualified as Aeson
import GenAlgebra qualified as Gen
import ParallelismLogic qualified as Parallelism
import ReportingLogic.Algebra qualified as ReportingLogic

-- * Error

-- | Application error.
data Error
  = GenError
      -- | Name of the artifact.
      Text
      -- | Name of the generator.
      Text
      -- | Version of the generator.
      Int
      -- | Details.
      Gen.Error
  | UnknownGenError
      -- | Name of the artifact.
      Text
      -- | Name of the generator.
      Text
      -- | Version of the generator.
      Int
  | GenConfigParsingError
      -- | Name of the artifact.
      Text
      -- | Name of the generator.
      Text
      -- | Version of the generator.
      Int
      -- | Error message.
      Text
  | MigrationsError Migrations.Error

-- * States

data ProjectFileLoaded = ProjectFileLoaded
  { configFilePath :: Path,
    name :: Gen.Name,
    version :: NonEmpty Int,
    -- | Path to the directory with migrations.
    migrationsDir :: Path,
    -- | Path to the directory with queries.
    queriesDir :: Path,
    -- | List of codegen configurations by their versions and names.
    artifacts :: [(Text, Text, Int, Aeson.Value)]
  }

data QueriesLoaded

type QueriesIntrospected = [QueryIntrospected]

data QueriesMetadataLoaded

type QueriesListed = [QueryListed]

data QueryListed = QueryListed
  { name :: Gen.Name,
    filePath :: Path,
    signatureFilePath :: Maybe Path
  }

data QuerySqlLoaded = QuerySqlLoaded
  { sql :: Text
  }

data QuerySqlParsed = QuerySqlParsed
  {
  }

data QueryIntrospected = QueryIntrospected
  { query :: Gen.Query,
    mentionedCustomTypes :: Map Gen.Name Gen.CustomType
  }

data CodeGenerated = CodeGenerated
  { artifacts :: [CodeGeneratedArtifact]
  }

data CodeGeneratedArtifact = CodeGeneratedArtifact
  { name :: Text,
    filePaths :: [Path],
    replaced :: Bool
  }

data SignatureGenerated = SignatureGenerated
  { filePath :: Path,
    replaced :: Bool
  }

data QuerySignatureLoaded
  = NotFoundQuerySignatureLoaded
  | QuerySignatureLoaded
      -- | Parameters of the query.
      (NonEmpty (Gen.Name, Gen.Type))
      -- | Result of the query.
      Gen.QueryResult

type QueriesMetadataMerged = Map Gen.Name QueryIntrospected

-- * Effect

class
  ( MonadError Error m,
    MonadReader [Gen.Gen] m,
    Parallelism.Parallelism m,
    ReportingLogic.Reports m,
    Migrations.ControlsMigrations Error m
  ) =>
  Effect m
  where
  loadProjectFile :: m ProjectFileLoaded
  listQueries :: ProjectFileLoaded -> m QueriesListed
  loadQuerySql :: QueryListed -> m QuerySqlLoaded

  -- | Attempt to load the query signature file.
  --
  -- Missing file is not an error. Parsing failure of an existing file however is.
  loadQuerySignature :: ProjectFileLoaded -> QueryListed -> m QuerySignatureLoaded

  parseQuerySql :: QuerySqlLoaded -> m QuerySqlParsed
  introspectQuery :: QuerySqlParsed -> m QueryIntrospected
  mergeQueryMetadata :: QueryIntrospected -> QuerySignatureLoaded -> m QueryIntrospected

  -- | Create or replace the signature file for the query.
  generateSignature :: ProjectFileLoaded -> QueryIntrospected -> m SignatureGenerated

  writeFile :: Path -> Text -> m ()
