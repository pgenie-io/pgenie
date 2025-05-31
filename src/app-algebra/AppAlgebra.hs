{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module AppAlgebra where

import Base.Prelude hiding (writeFile)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict qualified as Map
import GenAlgebra qualified as Gen
import System.FilePath qualified as FilePath

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

-- * Ops

check :: (Effect m) => m ()
check = do
  projectFileLoaded <- loadProjectFile
  analyse projectFileLoaded
  pure ()

generate :: (Effect m) => m ()
generate = do
  projectFileLoaded <- loadProjectFile
  queriesMetadataMerged <- analyse projectFileLoaded
  genProject <- assembleGenProject projectFileLoaded queriesMetadataMerged
  generateCode projectFileLoaded genProject
  pure ()

extractAllMentionedCustomTypes :: (Effect m) => QueriesMetadataMerged -> m (Map Gen.Name Gen.CustomType)
extractAllMentionedCustomTypes = foldM step Map.empty . Map.toList
  where
    step map (queryName, queryIntrospected) =
      foldM step' map (Map.toList queryIntrospected.mentionedCustomTypes)
      where
        step' map (name, customType) =
          case Map.lookup name map of
            Just _ -> error "TODO: handle duplicate custom types"
            Nothing -> do
              pure (Map.insert name customType map)

assembleGenProject :: (Effect m) => ProjectFileLoaded -> QueriesMetadataMerged -> m Gen.Project
assembleGenProject projectFileLoaded queriesMetadataMerged = do
  mentionedCustomTypes <- extractAllMentionedCustomTypes queriesMetadataMerged
  pure
    Gen.Project
      { Gen.name = projectFileLoaded.name,
        Gen.version = projectFileLoaded.version,
        Gen.customTypes = mentionedCustomTypes,
        Gen.queries = Map.map (.query) queriesMetadataMerged
      }

generateCode :: (Effect m) => ProjectFileLoaded -> Gen.Project -> m CodeGenerated
generateCode projectFileLoaded genProject = do
  gensAvail <- ask
  let gensAvailMap =
        gensAvail
          & fmap (\gen -> ((gen.configSectionKey, gen.version), gen))
          & Map.fromList
  artifacts <- forM projectFileLoaded.artifacts \(artifactName, genName, genVersion, genConfigJson) -> do
    Gen.Gen configSectionKey version generatorConfigParser generate <- case Map.lookup (genName, genVersion) gensAvailMap of
      Nothing ->
        throwError (UnknownGenError artifactName genName genVersion)
      Just gen ->
        pure gen
    genConfig <- case Aeson.parse generatorConfigParser genConfigJson of
      Aeson.Error errString ->
        throwError (GenConfigParsingError artifactName genName genVersion (onto errString))
      Aeson.Success genConfigParsed ->
        pure genConfigParsed
    case generate genConfig genProject of
      Left err ->
        throwError (GenError artifactName genName genVersion err)
      Right generatedFiles -> do
        let artifactPath = to @FilePath artifactName
        -- TODO: check if the artifact path exists, create it if not
        overwriting <- pure False
        generatedFilePaths <- for generatedFiles \(path, content) -> do
          let modifiedPath = FilePath.combine artifactPath path
          writeFile modifiedPath content
          pure modifiedPath
        pure (CodeGeneratedArtifact artifactName generatedFilePaths overwriting)
  pure (CodeGenerated artifacts)

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
      Map.fromList
        <$> for queriesListed \queryListed -> parallelly do
          (queryIntrospected, querySignatureLoaded) <- runParallelly \parallelly ->
            (,)
              <$> parallelly do
                querySqlLoaded <- loadQuerySql queryListed
                querySqlParsed <- parseQuerySql querySqlLoaded
                introspectQuery temporaryDbCreated querySqlParsed
              <*> parallelly do
                loadQuerySignature projectFileLoaded queryListed
          merged <- mergeQueryMetadata queryIntrospected querySignatureLoaded
          pure (queryListed.name, merged)
