module Logic
  ( analyse,
    generate,
    manageIndexes,
    module Logic.Algebra,
    module Logic.IndexOptimizer,
    module Logic.SeqScanDetector,
  )
where

import AlgebraicPath qualified as Path
import Base.Prelude hiding (readFile, writeFile)
import Control.Monad.Parallel qualified as MonadParallel
import Data.Aeson.Text qualified as Aeson.Text
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Dhall.Core qualified as Dhall
import Dhall.Marshal.Encode qualified as Dhall
import Logic.Algebra
import Logic.CodeGen (generateCode)
import Logic.Dsl
import Logic.IndexOptimizer
import Logic.IndexOptimizer qualified as IndexOptimizer
import Logic.Name qualified as Name
import Logic.ProjectFile qualified as ProjectFile
import Logic.SeqScanDetector
import Logic.SeqScanDetector qualified as SeqScanDetector
import Logic.SignatureFile qualified as SignatureFile
import Logic.SqlTemplate qualified as SqlTemplate
import Logic.SyntaxAnalyser qualified as SyntaxAnalyser
import PGenieGen.Model.Input qualified as Gen.Input
import SyntacticClass qualified as Syntactic

-- * Intermediate (non-interface) Types

data QueryListed = QueryListed
  { name :: Name.Name,
    filePath :: Path,
    signatureFilePath :: Maybe Path
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

-- | Validate the project and optionally output the model.
--
-- Performs the same analysis and emits the same seq-scan warnings as
-- 'generate', but does not write any artifact files.  When a format is
-- provided the project model is also serialised and returned so the caller
-- can write it to stdout.
analyse :: (Caps m) => AnalyseOptions -> Maybe ModelFormat -> m Text
analyse options maybeFormat =
  run do
    stage "" 2 do
      projectFile <- loadProjectFile
      (genProject, seqScanFindings, _indexes) <- analyseProject projectFile
      unless (null seqScanFindings) do
        for_ seqScanFindings \(queryName, finding) ->
          warn
            ( Error
                []
                ( "Sequential scan detected in query '"
                    <> queryName
                    <> "': table '"
                    <> finding.tableName
                    <> "' scanned without index on ("
                    <> Text.intercalate ", " finding.suggestedIndexColumns
                    <> ")"
                )
                (Just "Run 'manage-indexes' to generate index migration")
                [("query", queryName), ("table", finding.tableName)]
            )
        when options.failOnSeqScans do
          throwError
            ( Error
                []
                "Sequential scans detected"
                (Just "Run 'manage-indexes' to generate index migration, or remove --fail-on-seq-scans to allow warnings")
                []
            )
      case maybeFormat of
        Nothing -> pure ""
        Just ModelFormatDhall -> pure (Dhall.pretty (Dhall.cse (Dhall.denote (Dhall.inject.embed genProject))))
        Just ModelFormatJson -> pure (to (Aeson.Text.encodeToTextBuilder genProject))

generate :: (Caps m) => GenerateOptions -> m ()
generate options =
  run do
    stage "" 2 do
      projectFile <- loadProjectFile
      (genProject, seqScanFindings, _indexes) <- analyseProject projectFile
      unless (null seqScanFindings) do
        for_ seqScanFindings \(queryName, finding) ->
          warn
            ( Error
                []
                ( "Sequential scan detected in query '"
                    <> queryName
                    <> "': table '"
                    <> finding.tableName
                    <> "' scanned without index on ("
                    <> Text.intercalate ", " finding.suggestedIndexColumns
                    <> ")"
                )
                (Just "Run 'manage-indexes' to generate index migration")
                [("query", queryName), ("table", finding.tableName)]
            )
        when options.failOnSeqScans do
          throwError
            ( Error
                []
                "Sequential scans detected"
                (Just "Run 'manage-indexes' to generate index migration, or remove --fail-on-seq-scans to allow warnings")
                []
            )
      generateCode projectFile genProject
      pure ()

-- | Analyse the project's index usage and output the recommended migration SQL
-- to stdout. When @--write-file@ is set, also writes the migration to a
-- numbered file in the @migrations/@ directory.
manageIndexes :: (Caps m) => ManageIndexesOptions -> m Text
manageIndexes options =
  run do
    stage "" 2 do
      projectFile <- loadProjectFile
      (_genProject, seqScanFindings, indexes) <- analyseProject projectFile
      handleIndexOptimization options indexes seqScanFindings

-- * Helpers

loadProjectFile :: Script ProjectFile.ProjectFile
loadProjectFile = do
  configContent <- readFile "project1.pgn.yaml"
  ProjectFile.tryFromYaml configContent

loadQuerySql :: QueryListed -> Script SqlTemplate.SqlTemplate
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

analyseProject :: ProjectFile.ProjectFile -> Script (Gen.Input.Project, [(Text, SeqScanFinding)], [IndexInfo])
analyseProject projectFile =
  stage "Analysing" 2 do
    stage "Migrations" 2 do
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

    -- Fetch existing indexes after migrations have been applied
    indexes <-
      stage "Checking indexes" 0 do
        getIndexes

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

    (queries, customTypes, seqScanFindings) <-
      stage "Queries" (length queriesListed) do
        mixedList <-
          MonadParallel.forM queriesListed \queryListed ->
            stage (Name.inSnakeCase queryListed.name) 2 do
              sqlTemplate <-
                stage "Loading" 0 do
                  loadQuerySql queryListed

              let nativeTemplate =
                    sqlTemplate
                      & SqlTemplate.render
                        True
                        (\_ x -> "$" <> Syntactic.toTextBuilder (succ x))
                      & to

              InferredQueryTypes {params, resultColumns, mentionedCustomTypes} <-
                stage "Inferring" 0 do
                  (queryTypes, warnings) <- inferQueryTypes nativeTemplate
                  for warnings warn
                  pure queryTypes

              -- Detect sequential scans via EXPLAIN
              querySeqScanFindings <-
                catchError
                  ( do
                      explainLines <- explainQuery nativeTemplate
                      let findings = SeqScanDetector.detectSeqScans explainLines
                      pure (map (Name.inSnakeCase queryListed.name,) findings)
                  )
                  (\_ -> pure [])

              let fallbackSeqScanFindings =
                    SeqScanDetector.inferSeqScanFindingsFromSql nativeTemplate
                      & map (Name.inSnakeCase queryListed.name,)
                  effectiveSeqScanFindings =
                    if null querySeqScanFindings
                      then fallbackSeqScanFindings
                      else querySeqScanFindings

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

              -- Signature file handling
              let sigPath = SignatureFile.signatureFilePath queryListed.filePath
                  inferredSig = SignatureFile.fromInferred interpretedParams result

              (finalParams, finalResult) <- do
                maybeSigContent <-
                  catchError
                    (Just <$> readFile sigPath)
                    (\(_ :: Error) -> pure Nothing)
                case maybeSigContent of
                  Nothing -> do
                    writeFile sigPath (SignatureFile.serialize inferredSig)
                    pure (interpretedParams, result)
                  Just sigContent -> do
                    fileSig <- case SignatureFile.tryParse sigContent of
                      Left err ->
                        throwError
                          ( Error
                              []
                              "Failed to parse signature file"
                              (Just "Check the YAML syntax in the signature file")
                              [("file", Path.toText sigPath), ("error", err)]
                          )
                      Right sig -> pure sig
                    case SignatureFile.validateAndMerge inferredSig fileSig of
                      Left err -> throwError err
                      Right mergedSig ->
                        pure (SignatureFile.applyToQuery mergedSig interpretedParams result)

              pure
                ( Gen.Input.Query
                    { name = Name.toGenName queryListed.name,
                      srcPath = queryListed.filePath,
                      params = finalParams,
                      result = finalResult,
                      fragments = SqlTemplate.toGenQueryFragments sqlTemplate
                    },
                  mentionedCustomTypes,
                  effectiveSeqScanFindings
                )

        let (queries, customTypesDump, seqScanFindingsDump) = unzip3 mixedList
            customTypes =
              customTypesDump
                & concat
                & fmap (\x -> ((x.pgSchema, x.pgName), x))
                & Map.fromList
                & Map.elems
            seqScanFindings = concat seqScanFindingsDump

        pure (queries, customTypes, seqScanFindings)

    pure
      ( Gen.Input.Project
          { space = Name.toGenName projectFile.space,
            name = Name.toGenName projectFile.name,
            version = projectFile.version,
            customTypes = customTypes,
            queries = queries
          },
        seqScanFindings,
        indexes
      )

stagedParFor :: Text -> (a -> Text) -> [a] -> (a -> Script b) -> Script [b]
stagedParFor stageName nameFn items action =
  stage stageName (length items) do
    MonadParallel.forM items \item ->
      stage (nameFn item) 0 do
        action item

-- | Depending on the warning handling strategy this can either log the warning and continue or throw an error to stop the execution.
warn :: Error -> Script ()
warn =
  -- TODO: Implement conditional throwing or emission.
  emit . WarningEmitted

-- | Run the unified index optimizer, combining redundant-index detection,
-- excessive-composite narrowing, and missing-index creation into one step.
-- Returns the migration SQL, printing it to stdout via the caller.
-- When @--allow-redundant-indexes@ is set, DropIndex actions are emitted as
-- warnings instead of being included in the generated migration.
-- When @--write-file@ is set, also writes the migration to a numbered file in
-- @migrations/@, failing if the existing files do not follow the @N.sql@
-- naming convention.
handleIndexOptimization :: ManageIndexesOptions -> [IndexInfo] -> [(Text, SeqScanFinding)] -> Script Text
handleIndexOptimization options indexes seqScanFindings = do
  let queryNeeds =
        map (\(_, finding) -> (finding.tableName, finding.suggestedIndexColumns)) seqScanFindings
      allActions = IndexOptimizer.optimizeIndexes indexes queryNeeds
      (dropActions, createActions) = partition isDropAction allActions
      migrationActions =
        if options.allowRedundantIndexes
          then createActions
          else allActions
  when options.allowRedundantIndexes do
    for_ dropActions \action ->
      warn (Error [] (indexActionMessage action) (Just "Suppressed by --allow-redundant-indexes") (indexActionDetails action))
  if null migrationActions
    then pure ""
    else do
      let migrationContent = IndexOptimizer.generateMigration migrationActions
      when options.writeToFile do
        writeMigrationFile migrationContent
      pure migrationContent

-- | Write the migration SQL to a numbered file in @migrations/@.
-- Fails explicitly if any existing file does not follow the @N.sql@ naming
-- convention (i.e. the base name is not a sequence of digits).
writeMigrationFile :: Text -> Script ()
writeMigrationFile content = do
  migrationsListed <-
    listDir "migrations"
      & fmap (filter (\p -> Path.toExtensions p == ["sql"]))
      & fmap sort
  for_ migrationsListed \p -> do
    let baseName = Path.toBasename p
    unless (not (Text.null baseName) && Text.all isDigit baseName) do
      throwError
        ( Error
            []
            ("Migration naming convention is not clear: " <> Path.toText p)
            (Just "All migration files must follow the N.sql naming convention (e.g., 1.sql, 2.sql)")
            [("file", Path.toText p)]
        )
  let nextMigrationNum = length migrationsListed + 1
      migrationFileName = Text.pack (show nextMigrationNum) <> ".sql"
  case Path.maybeFromText ("migrations/" <> migrationFileName) of
    Nothing ->
      throwError
        ( Error
            []
            "Failed to construct migration file path"
            Nothing
            [("filename", migrationFileName)]
        )
    Just migrationPath -> do
      writeFile migrationPath content
      emit
        ( WarningEmitted
            ( Error
                []
                ("Written migration to migrations/" <> migrationFileName)
                (Just "Review the migration and commit it")
                []
            )
        )

isDropAction :: IndexAction -> Bool
isDropAction (DropIndex {}) = True
isDropAction (CreateIndex {}) = False

indexActionMessage :: IndexAction -> Text
indexActionMessage (DropIndex idx reason) = case reason of
  ExactDuplicate other ->
    "Redundant index: "
      <> idx.indexName
      <> " on "
      <> idx.tableName
      <> " is an exact duplicate of "
      <> other.indexName
  PrefixRedundancy other ->
    "Redundant index: "
      <> idx.indexName
      <> " on "
      <> idx.tableName
      <> " is a prefix of "
      <> other.indexName
  ExcessiveComposite replacement ->
    "Excessive composite index: "
      <> idx.indexName
      <> " on "
      <> idx.tableName
      <> " can be narrowed to ("
      <> Text.intercalate ", " replacement
      <> ")"
  UnusedByQueries ->
    "Redundant index: "
      <> idx.indexName
      <> " on "
      <> idx.tableName
      <> " is not used by observed query needs"
indexActionMessage (CreateIndex tbl cols) =
  "Missing index: "
    <> tbl
    <> " needs an index on ("
    <> Text.intercalate ", " cols
    <> ")"

indexActionDetails :: IndexAction -> [(Text, Text)]
indexActionDetails (DropIndex idx _) = [("index", idx.indexName), ("table", idx.tableName)]
indexActionDetails (CreateIndex tbl cols) = [("table", tbl), ("columns", Text.intercalate ", " cols)]
