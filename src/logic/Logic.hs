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
import Logic.Name qualified as Name
import Logic.SqlTemplate qualified as SqlTemplate
import Logic.SyntaxAnalyser qualified as SyntaxAnalyser
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import PGenieGen.Model.Output.Report qualified as Gen.Output.Report
import SyntacticClass qualified as Syntactic

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
  loadGen genLocation = lift (loadGen genLocation)

instance (Emits m) => Emits (Logic m) where
  emit event = lift (emit event)

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
    stage "" 2 do
      projectFileLoaded <- loadProjectFile
      genProject <- do
        analyse projectFileLoaded
      generateCode projectFileLoaded genProject
      pure ()

-- * Helpers

loadProjectFile :: (FsOps m) => m ProjectFileLoaded
loadProjectFile = do
  _configContent <- readFile "project.pgn1.yaml"
  -- TODO: Parse YAML config and extract project details
  -- For now return placeholder
  throwError
    ( Error
        []
        "Project file parsing is not yet implemented"
        (Just "Implement YAML parsing for project.pgn1.yaml")
        []
    )

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

generateCode :: (LoadsGen m, Stages m, MonadParallel m, FsOps m) => ProjectFileLoaded -> Gen.Input.Project -> m [GeneratedArtifact]
generateCode projectFileLoaded project =
  stage "Generating code" (length projectFileLoaded.artifacts) do
    MonadParallel.forM projectFileLoaded.artifacts \(Artifact {..}) ->
      stage name 2 do
        compileFn <-
          stage "Loading generator" 1 do
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
                pure compileFn

        stage "Compiling" 1 do
          let output = compileFn project
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
              let artifactPath = fold (Path.maybeFromText name)
              generatedFilePaths <- for generatedFiles \file -> do
                let modifiedPath = artifactPath <> file.path
                writeFile modifiedPath file.content
                pure modifiedPath
              pure (GeneratedArtifact name output.warnings generatedFilePaths)

analyse :: (LoadsGen m, DbOps m, MonadParallel m, FsOps m, Stages m, Emits m) => ProjectFileLoaded -> m Gen.Input.Project
analyse projectFileLoaded =
  stage "Analysing" 2 do
    stage "Executing migrations" 2 do
      migrationsListed <-
        listDir projectFileLoaded.migrationsDir
          & fmap (filter (\p -> Path.toExtensions p == ["sql"]))
          & fmap sort

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
        listDir projectFileLoaded.queriesDir

      let queryPaths =
            allPathsInQueriesDir
              & filter (\p -> Path.toExtensions p == ["sql"])
              & sort

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
                stage "Reading file" 1 do
                  loadQuerySql queryListed

              let nativeTemplate =
                    sqlTemplate
                      & SqlTemplate.render
                        True
                        (\_ x -> "$" <> Syntactic.toTextBuilder (succ x))
                      & to

              InferredQueryTypes {params, resultColumns, mentionedCustomTypes} <-
                stage "Inferring types" 1 do
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
        { owner = Name.toGenName projectFileLoaded.owner,
          name = Name.toGenName projectFileLoaded.name,
          version = projectFileLoaded.version,
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
