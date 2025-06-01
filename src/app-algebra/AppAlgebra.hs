module AppAlgebra where

import Base.Prelude hiding (writeFile)
import Data.Aeson qualified as Aeson
import GenAlgebra qualified as Gen

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

-- * States

data ProjectFileLoaded = ProjectFileLoaded
  { configFilePath :: FilePath,
    name :: Gen.Name,
    version :: NonEmpty Int,
    -- | List of codegen configurations by their versions and names.
    artifacts :: [(Text, Text, Int, Aeson.Value)]
  }

data TemporaryDbCreated

data QueriesLoaded

type QueriesIntrospected = [QueryIntrospected]

data QueriesMetadataLoaded

type QueriesListed = [QueryListed]

data QueryListed = QueryListed
  { name :: Gen.Name,
    filePath :: FilePath,
    signatureFilePath :: Maybe FilePath
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
    filePaths :: [FilePath],
    replaced :: Bool
  }

data SignatureGenerated = SignatureGenerated
  { filePath :: FilePath,
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

type MigrationsListed = [MigrationListed]

data MigrationListed

data MigrationLoaded

data MigrationExecuted

-- * Effect

class (MonadError Error m, MonadReader [Gen.Gen] m) => Effect m where
  runParallelly :: (forall f. (Applicative f) => (forall a. m a -> f a) -> f a) -> m a
  loadProjectFile :: m ProjectFileLoaded
  createTemporaryDb :: ProjectFileLoaded -> m TemporaryDbCreated
  dropTemporaryDb :: TemporaryDbCreated -> m ()
  listMigrations :: ProjectFileLoaded -> m MigrationsListed
  loadMigration :: MigrationListed -> m MigrationLoaded
  executeMigration :: TemporaryDbCreated -> MigrationLoaded -> m MigrationExecuted
  listQueries :: ProjectFileLoaded -> m QueriesListed
  loadQuerySql :: QueryListed -> m QuerySqlLoaded

  -- | Attempt to load the query signature file.
  --
  -- Missing file is not an error. Parsing failure of an existing file however is.
  loadQuerySignature :: ProjectFileLoaded -> QueryListed -> m QuerySignatureLoaded

  parseQuerySql :: QuerySqlLoaded -> m QuerySqlParsed
  introspectQuery :: TemporaryDbCreated -> QuerySqlParsed -> m QueryIntrospected
  mergeQueryMetadata :: QueryIntrospected -> QuerySignatureLoaded -> m QueryIntrospected

  -- | Create or replace the signature file for the query.
  generateSignature :: ProjectFileLoaded -> QueryIntrospected -> m SignatureGenerated

  writeFile :: FilePath -> Text -> m ()
