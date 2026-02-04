{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module AppLogic where

import AlgebraicPath qualified as Path
import AppLogic.Migrations qualified as Migrations
import Base.Prelude hiding (writeFile)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict qualified as Map
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
  | MigrationsError Migrations.Error

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

data Url

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
  {
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

-- ** Instances

instance IsSome Error Migrations.Error where
  to = MigrationsError
  maybeFrom = \case
    MigrationsError err -> Just err
    _ -> Nothing

-- * Effect

class
  ( MonadError Error m,
    Parallelism m,
    Reports m,
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

  -- | Create or replace the signature file for the query.
  generateSignature :: ProjectFileLoaded -> QueryIntrospected -> m SignatureGenerated

  writeFile :: Path -> Text -> m ()

  loadGen :: Gen.Location -> m Gen

check :: (Effect m) => m ()
check = do
  projectFileLoaded <- loadProjectFile
  analyse projectFileLoaded
  pure ()

generate :: (Effect m) => m ()
generate =
  stage "Generate" 3 do
    projectFileLoaded <-
      stage "Load Project File" 1 do
        loadProjectFile
    genProject <- stage "Analyse" 1 do
      analyse projectFileLoaded
    stage "Generate Code" 1 do
      generateCode projectFileLoaded genProject
    pure ()

loadGens :: (Effect m) => [Artifact] -> m [(Text, Gen.Input -> Gen.Output)]
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

generateCode :: (Effect m) => ProjectFileLoaded -> Gen.Input.Project -> m CodeGenerated
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

analyse :: (Effect m) => ProjectFileLoaded -> m Gen.Input.Project
analyse projectFileLoaded = do
  Migrations.executeMigrationsAtPath projectFileLoaded.migrationsDir

  queriesListed <-
    listQueries projectFileLoaded

  queriesMerged <-
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
        queriesMerged & map (.query)

  let customTypes =
        queriesMerged
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

mergeQueryMetadata :: QueryIntrospected -> QuerySignatureLoaded -> m QueryIntrospected
mergeQueryMetadata =
  error "TODO"

stagedParFor :: (Effect m) => Text -> (a -> Text) -> [a] -> (a -> m b) -> m [b]
stagedParFor stageName nameFn items action =
  stage stageName (length items) do
    runParallelly do
      for items \item ->
        parallelly do
          stage (nameFn item) 0 do
            action item
