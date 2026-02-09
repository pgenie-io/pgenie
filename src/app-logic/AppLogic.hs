{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module AppLogic where

import AlgebraicPath qualified as Path
import Base.Prelude hiding (readFile, writeFile)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict qualified as Map
import FsAlgebra.Algebra qualified as FsAlgebra
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import ParallelismAlgebra
import StagingAlgebra

-- * Error

-- | Application error.
data Error
  = GenError
      -- | Name of the artifact.
      Text
      -- | Warnings.
      [Gen.Output.Report]
      -- | Error report.
      Gen.Output.Report
  | GenConfigParsingError
      -- | Name of the artifact.
      Text
      -- | Error message.
      Text
  | MigrationsError Error

-- * States

data ProjectFileLoaded = ProjectFileLoaded
  { configFilePath :: Path,
    owner :: Gen.Input.Name,
    name :: Gen.Input.Name,
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

type Gen = Gen.Gen

data QueriesLoaded

type QueriesIntrospected = [QueryIntrospected]

data QueriesMetadataLoaded

type QueriesListed = [QueryListed]

data QueryListed = QueryListed
  { name :: Gen.Input.Name,
    filePath :: Path,
    signatureFilePath :: Maybe Path
  }

data QuerySqlLoaded = QuerySqlLoaded
  { sql :: Text
  }

data QuerySqlParsed = QuerySqlParsed
  { sql :: Text
  }

data QueryIntrospected = QueryIntrospected
  { query :: Gen.Input.Query,
    mentionedCustomTypes :: [Gen.Input.CustomType]
  }

data CodeGenerated = CodeGenerated
  { artifacts :: [CodeGeneratedArtifact]
  }

data CodeGeneratedArtifact = CodeGeneratedArtifact
  { name :: Text,
    warnings :: [Gen.Output.Report],
    filePaths :: [Path]
  }

data SignatureGenerated = SignatureGenerated
  { filePath :: Path,
    replaced :: Bool
  }

data QuerySignatureLoaded
  = NotFoundQuerySignatureLoaded
  | QuerySignatureLoaded
      -- | Parameters of the query.
      [Gen.Input.Member]
      -- | Result of the query.
      (Maybe Gen.Input.ResultRows)

data QueriesMetadataMerged = QueriesMetadataMerged
  { queries :: [Gen.Input.Query],
    customTypes :: [Gen.Input.CustomType]
  }

type MigrationsLoaded = [MigrationLoaded]

type MigrationsExecuted = [MigrationExecuted]

data MigrationLoaded = MigrationLoaded
  { sql :: Text
  }

data MigrationExecuted = MigrationExecuted

-- * Transformers

-- ** EmittingEvents

runEmittingEvents :: EmittingEvents m a -> m a
runEmittingEvents (EmittingEvents runInner) = runInner 1.0

newtype EmittingEvents m a
  = EmittingEvents (Double -> m a)
  deriving
    (Functor, Applicative, Monad, Parallelism, FsOps, DbOps, LoadsGen)
    via (ReaderT Double m)

instance (MonadError Error m) => MonadError Error (EmittingEvents m) where
  throwError e = lift (throwError e)
  catchError (EmittingEvents f) handler =
    EmittingEvents \p -> catchError (f p) (\e -> let EmittingEvents h = handler e in h p)

instance (Reports m) => Stages (EmittingEvents m) where
  stage name substagesCount =
    if substagesCount > 0
      then \(EmittingEvents runInner) -> EmittingEvents \progressPerStage -> do
        enterStage name
        let progressPerSubstage = progressPerStage / fromIntegral substagesCount
        result <- runInner progressPerSubstage
        exitStage name 0
        pure result
      else \(EmittingEvents runInner) -> EmittingEvents \progressPerStage -> do
        enterStage name
        result <- runInner 0
        exitStage name progressPerStage
        pure result

instance MonadTrans EmittingEvents where
  lift ma = EmittingEvents \_ -> ma

class (Monad m) => Reports m where
  enterStage :: Text -> m ()
  exitStage :: Text -> Double -> m ()

-- * Effects

class (MonadError Error m) => DbOps m where
  executeMigration :: MigrationLoaded -> m MigrationExecuted
  introspectQuery :: QuerySqlParsed -> m QueryIntrospected

class (MonadError Error m) => FsOps m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()
  listDir :: Path -> m [Path]

-- | Domain operations.
class (MonadError Error m) => LoadsGen m where
  loadGen :: Gen.Location -> m Gen

type AllOps m =
  ( DbOps m,
    FsOps m,
    LoadsGen m,
    Parallelism m,
    Stages m,
    Reports m
  )

instance (DbOps m) => DbOps (ReaderT r m) where
  executeMigration migrationLoaded = lift (executeMigration migrationLoaded)
  introspectQuery querySqlParsed = lift (introspectQuery querySqlParsed)

instance (FsOps m) => FsOps (ReaderT r m) where
  readFile path = lift (readFile path)
  writeFile path content = lift (writeFile path content)
  listDir path = lift (listDir path)

instance (LoadsGen m) => LoadsGen (ReaderT r m) where
  loadGen genUrl = lift (loadGen genUrl)

check :: (LoadsGen m, DbOps m, Parallelism m, FsOps m, Reports m) => m ()
check = runEmittingEvents do
  projectFileLoaded <- loadProjectFile
  analyse projectFileLoaded
  pure ()

generate :: (LoadsGen m, DbOps m, Parallelism m, FsOps m, Reports m) => m ()
generate = runEmittingEvents do
  stage "Generate" 3 do
    projectFileLoaded <-
      stage "Load Project File" 1 do
        loadProjectFile
    genProject <- stage "Analyse" 1 do
      analyse projectFileLoaded
    stage "Generate Code" 1 do
      generateCode projectFileLoaded genProject
    pure ()

loadProjectFile :: (FsOps m) => m ProjectFileLoaded
loadProjectFile = do
  configContent <- readFile "project.pgn1.yaml"
  -- TODO: Parse YAML config and extract project details
  -- For now return placeholder
  error "TODO: Parse project config file"

loadQuerySql :: (FsOps m) => QueryListed -> m QuerySqlLoaded
loadQuerySql queryListed = do
  sql <- readFile queryListed.filePath
  pure QuerySqlLoaded {sql}

-- | Attempt to load the query signature file.
--
-- Missing file is not an error. Parsing failure of an existing file however is.
loadQuerySignature :: (FsOps m) => ProjectFileLoaded -> QueryListed -> m QuerySignatureLoaded
loadQuerySignature _projectFileLoaded queryListed = do
  case queryListed.signatureFilePath of
    Nothing -> pure NotFoundQuerySignatureLoaded
    Just sigPath -> do
      sigContent <- readFile sigPath
      -- TODO: Parse signature file content (JSON/YAML)
      -- For now, return not found
      pure NotFoundQuerySignatureLoaded

listQueries :: (FsOps m) => ProjectFileLoaded -> m QueriesListed
listQueries projectFileLoaded = do
  queryPaths <- listDir projectFileLoaded.queriesDir
  for queryPaths \queryPath -> do
    -- TODO: Extract proper query name from path
    -- For now use error as placeholder since this needs proper implementation
    error "TODO: Implement proper query name extraction from path"

loadGens :: (LoadsGen m, Stages m, Parallelism m) => [Artifact] -> m [(Text, Gen.Input -> Gen.Output)]
loadGens artifacts =
  stage "Loading generators" (length artifacts) do
    runParallelly do
      for artifacts \(Artifact {..}) ->
        parallelly do
          stage name 0 do
            gen <- loadGen genUrl
            case gen config of
              Left errMsg ->
                throwError (GenConfigParsingError name errMsg)
              Right compileFn ->
                pure (name, compileFn)

generateCode :: (LoadsGen m, Stages m, Parallelism m, FsOps m) => ProjectFileLoaded -> Gen.Input.Project -> m CodeGenerated
generateCode projectFileLoaded project = do
  loadedGens <- loadGens projectFileLoaded.artifacts

  artifacts <-
    stage "Compiling" (length loadedGens) do
      runParallelly do
        for loadedGens \(artifactName, compile) -> parallelly do
          stage artifactName 0 do
            let output = compile project
            case output.result of
              Gen.Output.ResultErr report ->
                throwError (GenError artifactName output.warnings report)
              Gen.Output.ResultOk generatedFiles -> do
                let artifactPath = fold (Path.maybeFromText artifactName)
                generatedFilePaths <- for generatedFiles \file -> do
                  let modifiedPath = artifactPath <> file.path
                  writeFile modifiedPath file.content
                  pure modifiedPath
                pure (CodeGeneratedArtifact artifactName output.warnings generatedFilePaths)

  pure (CodeGenerated artifacts)

-- | Create or replace the signature file for the query.
generateSignature :: (FsOps m) => ProjectFileLoaded -> QueryIntrospected -> m SignatureGenerated
generateSignature _projectFileLoaded queryIntrospected = do
  -- TODO: Implement proper signature generation
  error "TODO: Implement generateSignature"

-- TODO: implement in logic. This is not an integration layer concern.
parseQuerySql :: (Monad m) => QuerySqlLoaded -> m QuerySqlParsed
parseQuerySql querySqlLoaded = do
  -- For now, just pass through the SQL as-is
  -- In a real implementation, this might validate/parse the SQL
  pure QuerySqlParsed {sql = querySqlLoaded.sql}

analyse :: (LoadsGen m, DbOps m, Parallelism m, FsOps m, Stages m) => ProjectFileLoaded -> m Gen.Input.Project
analyse projectFileLoaded = do
  executeMigrationsAtPath projectFileLoaded.migrationsDir

  queriesListed <-
    listQueries projectFileLoaded

  queriesIntrospected <-
    runParallelly do
      for queriesListed \queryListed ->
        parallelly do
          (queryIntrospected, querySignatureLoaded) <-
            runParallelly do
              (,)
                <$> parallelly do
                  querySqlLoaded <- loadQuerySql queryListed
                  querySqlParsed <- parseQuerySql querySqlLoaded
                  introspectQuery querySqlParsed
                <*> parallelly do
                  loadQuerySignature projectFileLoaded queryListed

          mergeQueryMetadata queryIntrospected querySignatureLoaded

  let queries =
        queriesIntrospected & map (.query)

  let customTypes =
        queriesIntrospected
          & foldMap (.mentionedCustomTypes)
          & fmap (\x -> ((x.pgSchema, x.pgName), x))
          & Map.fromList
          & Map.elems

  pure
    Gen.Input.Project
      { owner = projectFileLoaded.owner,
        name = projectFileLoaded.name,
        version = projectFileLoaded.version,
        customTypes = customTypes,
        queries = queries
      }

mergeQueryMetadata :: (Monad m) => QueryIntrospected -> QuerySignatureLoaded -> m QueryIntrospected
mergeQueryMetadata queryIntrospected querySignatureLoaded = do
  case querySignatureLoaded of
    NotFoundQuerySignatureLoaded ->
      -- No signature file, return introspected data as-is
      pure queryIntrospected
    QuerySignatureLoaded params resultRows -> do
      -- TODO: Merge signature data with introspected data
      -- For now, prefer introspected data
      pure queryIntrospected

listMigrations :: (FsOps m) => Path -> m [Path]
listMigrations migrationsDir = do
  allPaths <- listDir migrationsDir
  -- Filter for .sql files
  let isSqlFile p = ".sql" `isSuffixOf` (to @String $ Path.toText p)
  pure $ filter isSqlFile allPaths

loadMigration :: (FsOps m) => Path -> m MigrationLoaded
loadMigration migrationPath = do
  sql <- readFile migrationPath
  pure MigrationLoaded {sql}

executeMigrationsAtPath ::
  (LoadsGen m, Parallelism m, DbOps m, FsOps m, Stages m) =>
  Path ->
  m MigrationsExecuted
executeMigrationsAtPath path =
  stage "Executing migrations" 2 do
    migrationsListed <- listMigrations path

    let migrationsCount = length migrationsListed

    migrationsLoaded <-
      stage "Loading" migrationsCount do
        runParallelly do
          for migrationsListed \migrationListed ->
            parallelly do
              stage (Path.toText migrationListed) 0 do
                migrationLoaded <- loadMigration migrationListed
                pure (migrationListed, migrationLoaded)

    stage "Executing" migrationsCount do
      for migrationsLoaded \(migrationListed, migrationLoaded) -> do
        stage (Path.toText migrationListed) 0 do
          executeMigration migrationLoaded

stagedParFor :: (LoadsGen m, Parallelism m, Stages m) => Text -> (a -> Text) -> [a] -> (a -> m b) -> m [b]
stagedParFor stageName nameFn items action =
  stage stageName (length items) do
    runParallelly do
      for items \item ->
        parallelly do
          stage (nameFn item) 0 do
            action item
