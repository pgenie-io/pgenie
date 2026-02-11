{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module Logic where

import AlgebraicPath qualified as Path
import Base.Prelude hiding (readFile, writeFile)
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import FsAlgebra.Algebra qualified as FsAlgebra
import Logic.SqlTemplate qualified as SqlTemplate
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import PGenieGen.Model.Output.Report qualified as Gen.Output.Report
import ParallelismAlgebra
import StagingAlgebra

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

type QueriesIntrospected = [QueryAnalysed]

data QueriesMetadataLoaded

data QueryListed = QueryListed
  { name :: Gen.Input.Name,
    filePath :: Path,
    signatureFilePath :: Maybe Path
  }

data QuerySqlLoaded = QuerySqlLoaded
  { sql :: Text
  }

data QueryAnalysed = QueryAnalysed
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

runLogic :: Logic m a -> m a
runLogic (Logic f) = f 0 []

-- |
-- Internal monad transformer for pure logic, which:
-- 
-- - Enables serialization of staging as reporting events
-- - Extends errors with staging paths to complete context
newtype Logic m a
  = Logic (Double -> [Text] -> m a)
  deriving
    (Functor, Applicative, Monad, Parallelism)
    via (ReaderT Double (ReaderT [Text] m))

instance MonadTrans Logic where
  lift ma = Logic \_ _ -> ma

instance (MonadError Error m) => MonadError Error (Logic m) where
  throwError e = Logic \_ path ->
    let newPath = path <> e.path
        newError = e {path = newPath}
     in throwError newError

  catchError (Logic f) handler =
    Logic \p q -> catchError (f p q) (\e -> let Logic h = handler e in h p q)

instance (Reports m) => Stages (Logic m) where
  stage name substagesCount (Logic runInner) =
    Logic \progressPerStage path ->
      if substagesCount > 0
        then do
          let newPath = name : path
          enterStage newPath
          let progressPerSubstage = progressPerStage / fromIntegral substagesCount
          result <- runInner progressPerSubstage newPath
          exitStage newPath 0
          pure result
        else do
          let newPath = name : path
          enterStage newPath
          result <- runInner 0 newPath
          exitStage newPath progressPerStage
          pure result

instance (DbOps m) => DbOps (Logic m) where
  executeMigration migrationLoaded = lift (executeMigration migrationLoaded)
  analyseQuery sqlTemplate = lift (analyseQuery sqlTemplate)

instance (FsOps m) => FsOps (Logic m) where
  readFile path = lift (readFile path)
  writeFile path content = lift (writeFile path content)
  listDir path = lift (listDir path)

instance (LoadsGen m) => LoadsGen (Logic m) where
  loadGen genUrl = lift (loadGen genUrl)

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
  executeMigration :: MigrationLoaded -> m MigrationExecuted
  analyseQuery :: SqlTemplate.SqlTemplate -> m QueryAnalysed

class (MonadError Error m) => FsOps m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()
  listDir :: Path -> m [Path]

-- | Domain operations.
class (MonadError Error m) => LoadsGen m where
  loadGen :: Gen.Location -> m Gen

-- | Combined capabilities required by the logic.
type Caps m =
  ( LoadsGen m,
    DbOps m,
    Parallelism m,
    FsOps m,
    Reports m
  )

-- * API ops

check :: (Caps m) => m ()
check =
  runLogic do
    projectFileLoaded <- loadProjectFile
    analyse projectFileLoaded
    pure ()

generate :: (Caps m) => m ()
generate =
  runLogic do
    stage "" 3 do
      projectFileLoaded <-
        stage "Loading Project File" 1 do
          loadProjectFile
      genProject <- stage "Analysing" 1 do
        analyse projectFileLoaded
      stage "Generating" 1 do
        generateCode projectFileLoaded genProject
      pure ()

-- * Helpers

loadProjectFile :: (FsOps m) => m ProjectFileLoaded
loadProjectFile = do
  configContent <- readFile "project.pgn1.yaml"
  -- TODO: Parse YAML config and extract project details
  -- For now return placeholder
  error "TODO: Parse project config file"

loadQuerySql :: (FsOps m) => QueryListed -> m SqlTemplate.SqlTemplate
loadQuerySql queryListed = do
  sql <- readFile queryListed.filePath
  case SqlTemplate.tryFromText sql of
    Left err -> error "TODO"
    Right res -> pure res

-- | Attempt to load the query signature file.
--
-- Missing file is not an error. Parsing failure of an existing file however is.
loadQuerySignature :: (FsOps m) => ProjectFileLoaded -> QueryListed -> m QuerySignatureLoaded
loadQuerySignature _projectFileLoaded queryListed = do
  case queryListed.signatureFilePath of
    Nothing -> pure NotFoundQuerySignatureLoaded
    Just sigPath -> do
      sigContent <- readFile sigPath
      error "TODO: Parse signature file content (JSON/YAML)"

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
                throwError
                  ( Error
                      []
                      errMsg
                      (Just "Ensure the artifact configuration conforms to the format expected by the generator")
                      [ ("config", to (Aeson.encodeToTextBuilder config))
                      ]
                  )
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
                throwError
                  ( Error
                      report.path
                      report.message
                      Nothing
                      [ ( "warnings",
                          output.warnings
                            & map Gen.Output.Report.toWarningYamlText
                            & Text.intercalate "\n"
                        )
                      ]
                  )
              Gen.Output.ResultOk generatedFiles -> do
                let artifactPath = fold (Path.maybeFromText artifactName)
                generatedFilePaths <- for generatedFiles \file -> do
                  let modifiedPath = artifactPath <> file.path
                  writeFile modifiedPath file.content
                  pure modifiedPath
                pure (CodeGeneratedArtifact artifactName output.warnings generatedFilePaths)

  pure (CodeGenerated artifacts)

-- | Create or replace the signature file for the query.
generateSignature :: (FsOps m) => ProjectFileLoaded -> QueryAnalysed -> m SignatureGenerated
generateSignature _projectFileLoaded queryIntrospected = do
  -- TODO: Implement proper signature generation
  error "TODO: Implement generateSignature"

analyse :: (LoadsGen m, DbOps m, Parallelism m, FsOps m, Stages m) => ProjectFileLoaded -> m Gen.Input.Project
analyse projectFileLoaded = do
  stage "Executing migrations" 1 do
    executeMigrationsAtPath projectFileLoaded.migrationsDir

  queriesListed <-
    stage "Listing queries" 1 do
      queryPaths <- listDir projectFileLoaded.queriesDir
      for queryPaths \queryPath -> do
        -- TODO: Extract proper query name from path
        -- For now use error as placeholder since this needs proper implementation
        error "TODO: Implement proper query name extraction from path"

  queriesIntrospected <-
    stage "Introspecting queries" (length queriesListed) do
      runParallelly do
        for queriesListed \queryListed ->
          parallelly do
            stage "" 3 do
              (queryIntrospected, querySignatureLoaded) <-
                runParallelly do
                  (,)
                    <$> parallelly do
                      sqlTemplate <-
                        stage "loading" 1 do
                          loadQuerySql queryListed
                      stage "analysing" 1 do
                        analyseQuery sqlTemplate
                    <*> parallelly do
                      stage "signature-loading" 1 do
                        loadQuerySignature projectFileLoaded queryListed

              mergeQueryMetadata queryIntrospected querySignatureLoaded

  let queries =
        queriesIntrospected
          & map (.query)

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

mergeQueryMetadata :: (Monad m) => QueryAnalysed -> QuerySignatureLoaded -> m QueryAnalysed
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
