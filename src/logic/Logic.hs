{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module Logic
  ( check,
    generate,
    module Logic.Algebra,
  )
where

import AlgebraicPath qualified as Path
import Base.Prelude hiding (readFile, writeFile)
import Control.Monad.Parallel qualified as MonadParallel
import Data.Aeson.Text qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Logic.Algebra
import Logic.GeneratorHashes qualified as GeneratorHashes
import Logic.Name qualified as Name
import Logic.ProjectFile qualified as ProjectFile
import Logic.SqlTemplate qualified as SqlTemplate
import Logic.SyntaxAnalyser qualified as SyntaxAnalyser
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import PGenieGen.Model.Output.Report qualified as Gen.Output.Report
import SyntacticClass qualified as Syntactic

-- * Transformers

runLogic :: Logic m a -> m a
runLogic (Logic f) = f 1 []

-- |
-- Internal monad transformer for pure logic, which:
--
-- - Enables serialization of staging as reporting events
-- - Extends errors with staging paths to complete context
newtype Logic m a
  = Logic (Double -> [Text] -> m a)
  deriving
    (Functor, Applicative, Monad, MonadParallel)
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

instance (Emits m) => Stages (Logic m) where
  stage name substagesCount (Logic runInner) =
    Logic \progressPerStage path ->
      if substagesCount > 0
        then do
          let newPath = name : path
          emit (StageEntered newPath)
          let progressPerSubstage = progressPerStage / fromIntegral substagesCount
          result <- runInner progressPerSubstage newPath
          emit (StageExited newPath 0)
          pure result
        else do
          let newPath = name : path
          emit (StageEntered newPath)
          result <- runInner 0 newPath
          emit (StageExited newPath progressPerStage)
          pure result

instance (DbOps m) => DbOps (Logic m) where
  executeMigration migrationLoaded = lift (executeMigration migrationLoaded)
  inferQueryTypes sqlTemplate = lift (inferQueryTypes sqlTemplate)

instance (FsOps m) => FsOps (Logic m) where
  readFile path = lift (readFile path)
  writeFile path content = lift (writeFile path content)
  listDir path = lift (listDir path)

instance (LoadsGen m) => LoadsGen (Logic m) where
  loadGen genLocation maybeHash = lift (loadGen genLocation maybeHash)

instance (Emits m) => Emits (Logic m) where
  emit event = lift (emit event)

-- * Intermediate (non-interface) Types

data QueryListed = QueryListed
  { name :: Name.Name,
    filePath :: Path,
    signatureFilePath :: Maybe Path
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

-- * API ops

check :: (Caps m) => m ()
check =
  runLogic do
    projectFile <- loadProjectFile
    analyse projectFile
    pure ()

generate :: (Caps m) => m ()
generate =
  runLogic do
    stage "Generate" 2 do
      projectFile <- loadProjectFile
      genProject <- do
        analyse projectFile
      generateCode projectFile genProject
      pure ()

-- * Helpers

locationToUrl :: Gen.Location -> Text
locationToUrl = \case
  Gen.LocationUrl url -> url
  Gen.LocationPath path -> Path.toText path

loadProjectFile :: (FsOps m) => m ProjectFile.ProjectFile
loadProjectFile = do
  configContent <- readFile "project.pgn1.yaml"
  ProjectFile.tryFromYaml configContent

loadQuerySql :: (FsOps m) => QueryListed -> m SqlTemplate.SqlTemplate
loadQuerySql queryListed = do
  sql <- readFile queryListed.filePath
  case SqlTemplate.tryFromText sql of
    Left err ->
      throwError
        ( Error
            []
            "Failed to parse SQL template"
            (Just "Check the SQL syntax in the query file")
            [("file", Path.toText queryListed.filePath), ("error", to err)]
        )
    Right res -> pure res

generateCode :: (LoadsGen m, Stages m, MonadParallel m, FsOps m) => ProjectFile.ProjectFile -> Gen.Input.Project -> m [GeneratedArtifact]
generateCode projectFile project =
  stage "Generating code" (length projectFile.artifacts) do
    -- Load existing hashes file
    existingHashes <- GeneratorHashes.tryLoadHashesFile

    -- Load generators and collect new hashes
    artifactsWithHashes <-
      MonadParallel.forM projectFile.artifacts \artifact -> do
        let name = Name.inSnakeCase artifact.name
            genUrl = locationToUrl artifact.gen
            maybeHash = Map.lookup genUrl existingHashes
        stage name 2 do
          compileFnWithHash <-
            stage "Loading generator" 0 do
              (gen, newHash) <- loadGen artifact.gen maybeHash
              case gen artifact.config of
                Left errMsg ->
                  throwError
                    ( Error
                        []
                        errMsg
                        (Just "Ensure the artifact configuration conforms to the format expected by the generator")
                        [ ("config", to (Aeson.encodeToTextBuilder artifact.config))
                        ]
                    )
                Right compileFn ->
                  pure (compileFn, genUrl, newHash)

          stage "Compiling" 0 do
            let (compileFn, genUrl, newHash) = compileFnWithHash
                output = compileFn project
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
                artifactPath <- case Path.maybeFromText name of
                  Nothing ->
                    throwError
                      ( Error
                          []
                          "Invalid artifact name"
                          (Just "Must be in snake_case and must not start with a number")
                          [("name", name)]
                      )
                  Just path ->
                    pure ("artifacts" <> path)
                generatedFilePaths <- for generatedFiles \file -> do
                  let modifiedPath = artifactPath <> file.path
                  writeFile modifiedPath file.content
                  pure modifiedPath
                pure ((GeneratedArtifact name output.warnings generatedFilePaths), (genUrl, newHash))

    -- Extract artifacts and hashes
    let (artifacts, hashPairs) = unzip artifactsWithHashes
        updatedHashes = Map.union (Map.fromList hashPairs) existingHashes

    -- Write updated hashes file
    writeFile "freeze.pgn1.yaml" (GeneratorHashes.serializeHashesMap updatedHashes)

    pure artifacts

analyse :: (LoadsGen m, DbOps m, MonadParallel m, FsOps m, Stages m, Emits m) => ProjectFile.ProjectFile -> m Gen.Input.Project
analyse projectFile =
  stage "Analysing" 2 do
    stage "Migrating" 2 do
      migrationsListed <-
        listDir "migrations"
          & fmap (filter (\p -> Path.toExtensions p == ["sql"]))
          & fmap sort
          & fmap (fmap ("migrations" <>))

      let migrationsCount = length migrationsListed

      migrationsLoaded <-
        stage "Loading" migrationsCount do
          MonadParallel.forM migrationsListed \migrationListed -> do
            stage (Path.toText migrationListed) 0 do
              migrationLoaded <- readFile migrationListed
              pure (migrationListed, migrationLoaded)

      stage "Executing" migrationsCount do
        for migrationsLoaded \(migrationListed, migrationLoaded) -> do
          stage (Path.toText migrationListed) 0 do
            executeMigration migrationLoaded

    queriesListed <- do
      allPathsInQueriesDir <-
        listDir "queries"
          & fmap sort
          & fmap (fmap ("queries" <>))

      let queryPaths =
            allPathsInQueriesDir
              & filter (\p -> Path.toExtensions p == ["sql"])

      for queryPaths \queryPath -> do
        -- Extract query name from path by taking the file name without extension
        name <- case Name.tryFromText (Path.toBasename queryPath) of
          Left err ->
            throwError
              ( Error
                  []
                  "Failed to extract query name from path"
                  (Just "Ensure the query file name is a valid identifier")
                  [ ("file", Path.toText queryPath),
                    ("error", err)
                  ]
              )
          Right name ->
            pure name

        pure
          QueryListed
            { name = name,
              filePath = queryPath,
              signatureFilePath = Nothing
            }

    (queries, customTypes) <-
      stage "Analysing queries" (length queriesListed) do
        mixedList <-
          MonadParallel.forM queriesListed \queryListed ->
            stage (Name.inSnakeCase queryListed.name) 2 do
              sqlTemplate <-
                stage "Reading file" 0 do
                  loadQuerySql queryListed

              let nativeTemplate =
                    sqlTemplate
                      & SqlTemplate.render
                        True
                        (\_ x -> "$" <> Syntactic.toTextBuilder (succ x))
                      & to

              InferredQueryTypes {params, resultColumns, mentionedCustomTypes} <-
                stage "Inferring types" 0 do
                  (queryTypes, warnings) <- inferQueryTypes nativeTemplate
                  for warnings warn
                  pure queryTypes

              result :: Maybe Gen.Input.ResultRows <-
                let byCardinality cardinality =
                      pure case nonEmpty resultColumns of
                        Nothing ->
                          Nothing
                        Just columns ->
                          Just (Gen.Input.ResultRows cardinality columns)
                 in case SyntaxAnalyser.resolveText nativeTemplate of
                      Left err -> do
                        warn
                          ( Error
                              []
                              "Failed to detect result cardinality by AST. Defaulting to multi-row"
                              Nothing
                              [("error", err)]
                          )
                        byCardinality Gen.Input.ResultRowsCardinalityMultiple
                      Right SyntaxAnalyser.QuerySyntaxAnalysis {resultRowAmount} ->
                        case resultRowAmount of
                          SyntaxAnalyser.SpecificRowAmount 0 ->
                            pure Nothing
                          SyntaxAnalyser.SpecificRowAmount 1 ->
                            byCardinality Gen.Input.ResultRowsCardinalitySingle
                          SyntaxAnalyser.SpecificRowAmount _ ->
                            byCardinality Gen.Input.ResultRowsCardinalityMultiple
                          SyntaxAnalyser.UpToRowAmount 0 ->
                            pure Nothing
                          SyntaxAnalyser.UpToRowAmount 1 ->
                            byCardinality Gen.Input.ResultRowsCardinalityOptional
                          SyntaxAnalyser.UpToRowAmount _ ->
                            byCardinality Gen.Input.ResultRowsCardinalityMultiple
                          SyntaxAnalyser.AnyRowAmount ->
                            byCardinality Gen.Input.ResultRowsCardinalityMultiple

              let interpretedParams =
                    zipWith
                      ( \param name ->
                          Gen.Input.Member
                            { name = Name.toGenName name,
                              pgName = Name.inSnakeCase name,
                              isNullable = param.isNullable,
                              value = param.type_
                            }
                      )
                      params
                      (SqlTemplate.toGenParamNames sqlTemplate)

              pure
                ( Gen.Input.Query
                    { name = Name.toGenName queryListed.name,
                      srcPath = queryListed.filePath,
                      params = interpretedParams,
                      result,
                      fragments = SqlTemplate.toGenQueryFragments sqlTemplate
                    },
                  mentionedCustomTypes
                )

        let (queries, customTypesDump) = unzip mixedList
            customTypes =
              customTypesDump
                & concat
                & fmap (\x -> ((x.pgSchema, x.pgName), x))
                & Map.fromList
                & Map.elems

        pure (queries, customTypes)

    pure
      Gen.Input.Project
        { space = Name.toGenName projectFile.space,
          name = Name.toGenName projectFile.name,
          version = projectFile.version,
          customTypes = customTypes,
          queries = queries
        }

stagedParFor :: (LoadsGen m, MonadParallel m, Stages m) => Text -> (a -> Text) -> [a] -> (a -> m b) -> m [b]
stagedParFor stageName nameFn items action =
  stage stageName (length items) do
    MonadParallel.forM items \item ->
      stage (nameFn item) 0 do
        action item

-- | Depending on the warning handling strategy this can either log the warning and continue or throw an error to stop the execution.
warn :: (Emits m) => Error -> m ()
warn =
  -- TODO: Implement conditional throwing or emission.
  emit . WarningEmitted
