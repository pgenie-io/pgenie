{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module Logic.Algebra where

import Base.Prelude
import CodegenAlgebra qualified as Codegen

-- * Error

-- | Application error.
data Error
  = CodegenError Text Codegen.Error

-- * States

data ProjectFileLoaded = ProjectFileLoaded
  { configFilePath :: FilePath,
    codegens :: [[Codegen.QuerySignature] -> Either Codegen.Error Codegen.Artifact]
  }

data TemporaryDbCreated

data QueriesLoaded

type QueriesIntrospected = [QueryIntrospected]

data QueriesMetadataLoaded

type QueriesListed = [QueryListed]

data QueryListed = QueryListed
  { name :: Text,
    filePath :: FilePath,
    signatureFilePath :: Maybe FilePath
  }

data QuerySqlLoaded = QuerySqlLoaded
  { sql :: Text
  }

data QuerySqlParsed = QuerySqlParsed
  {
  }

data QueryIntrospected

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
  | QuerySignatureLoaded Codegen.QuerySignature

type QueriesMetadataMerged = [QueryMetadataMerged]

type QueryMetadataMerged = Codegen.QuerySignature

type MigrationsListed = [MigrationListed]

data MigrationListed

data MigrationLoaded

data MigrationExecuted

-- * Effect

class (MonadError Error m) => Effect m where
  runParallelly :: (forall f. (Applicative f) => (forall a. m a -> f a) -> f a) -> m a
  loadProjectFile :: [Codegen.Codegen] -> m ProjectFileLoaded
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
  mergeQueryMetadata :: QueryIntrospected -> QuerySignatureLoaded -> m QueryMetadataMerged
  generateCode :: ProjectFileLoaded -> QueriesMetadataMerged -> m CodeGenerated

  -- | Create or replace the signature file for the query.
  generateSignature :: ProjectFileLoaded -> QueryMetadataMerged -> m SignatureGenerated
