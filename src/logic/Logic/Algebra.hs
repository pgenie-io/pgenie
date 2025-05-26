{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module Logic.Algebra where

import Base.Prelude
import CodegenAlgebra qualified as Codegen
import Data.Aeson qualified as Aeson

-- * Error

-- | Application error.
data Error
  = CodegenError Text Codegen.Error

-- * States

data ProjectFileLoaded = ProjectFileLoaded
  { configFilePath :: FilePath,
    -- | List of codegen configurations.
    artifacts :: [(Text, Int, Aeson.Value)]
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
  mergeQueryMetadata :: QueryIntrospected -> QuerySignatureLoaded -> m QueryMetadataMerged
  generateCode :: [Codegen.Codegen] -> ProjectFileLoaded -> QueriesMetadataMerged -> m CodeGenerated

  -- | Create or replace the signature file for the query.
  generateSignature :: ProjectFileLoaded -> QueryMetadataMerged -> m SignatureGenerated

-- * Ops

check :: (Effect m) => m ()
check = do
  projectFileLoaded <- loadProjectFile
  analyse projectFileLoaded
  pure ()

generate :: (Effect m) => [Codegen.Codegen] -> m ()
generate codegens = do
  projectFileLoaded <- loadProjectFile
  queriesMetadataMerged <- analyse projectFileLoaded
  generateCode codegens projectFileLoaded queriesMetadataMerged
  pure ()

withTemporaryDb :: (Effect m) => ProjectFileLoaded -> (TemporaryDbCreated -> m a) -> m a
withTemporaryDb projectFileLoaded action = do
  temporaryDbCreated <- createTemporaryDb projectFileLoaded
  catchError
    (action temporaryDbCreated <* dropTemporaryDb temporaryDbCreated)
    (\err -> dropTemporaryDb temporaryDbCreated *> throwError err)

analyse :: (Effect m) => ProjectFileLoaded -> m QueriesMetadataMerged
analyse projectFileLoaded = do
  (migrationsListed, queriesListed) <-
    runParallelly \parallelly ->
      (,)
        <$> parallelly (listMigrations projectFileLoaded)
        <*> parallelly (listQueries projectFileLoaded)

  withTemporaryDb projectFileLoaded \temporaryDbCreated -> do
    forM_ migrationsListed \migrationListed -> do
      migrationLoaded <- loadMigration migrationListed
      executeMigration temporaryDbCreated migrationLoaded

    runParallelly \parallelly ->
      for queriesListed \queryListed -> parallelly do
        (queryIntrospected, querySignatureLoaded) <- runParallelly \parallelly ->
          (,)
            <$> parallelly do
              querySqlLoaded <- loadQuerySql queryListed
              querySqlParsed <- parseQuerySql querySqlLoaded
              introspectQuery temporaryDbCreated querySqlParsed
            <*> parallelly do
              loadQuerySignature projectFileLoaded queryListed
        mergeQueryMetadata queryIntrospected querySignatureLoaded
