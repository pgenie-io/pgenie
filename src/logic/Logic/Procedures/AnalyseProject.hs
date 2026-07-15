-- |
-- Loads a project's queries and migrations, runs migrations, infers query
-- types, checks index coverage, and detects sequential scans — the core
-- analysis pipeline shared by the @analyse@, @generate@, and
-- @manage-indexes@ commands.
module Logic.Procedures.AnalyseProject
  ( Port,
    Params (..),
    Result (..),
    run,
  )
where

import AlgebraicPath qualified as Path
import Control.Monad.Parallel qualified as MonadParallel
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GenBridge.Contract qualified as Gen.Input
import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.IndexCatalog (LoadsIndexes (..))
import Logic.Capabilities.Migrations (ExecutesMigrations (..))
import Logic.Capabilities.QueryAnalysis (InfersQueryTypes (..))
import Logic.Capabilities.Reporting (Warns (..))
import Logic.Capabilities.SeqScanExplain (ExplainsQuery (..))
import Logic.Capabilities.Staging (Stages (..))
import Logic.Domain.CustomTypeOrdering qualified as CustomTypeOrdering
import Logic.Domain.IndexOptimization (IndexInfo (..))
import Logic.Domain.Name qualified as Name
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.QueryAnalysis (InferredParam (..), InferredQueryTypes (..))
import Logic.Domain.QuerySignature qualified as QuerySignature
import Logic.Domain.Report (Report (..))
import Logic.Domain.SeqScanFinding (SeqScanFinding (..))
import Logic.Domain.SeqScanFinding qualified as SeqScanFinding
import Logic.Domain.SqlTemplate qualified as SqlTemplate
import Logic.Domain.SyntaxAnalyser qualified as SyntaxAnalyser
import Logic.Procedures.GenerateQuerySignatures qualified as GenerateQuerySigs
import Logic.Procedures.GenerateTypeSignatures qualified as GenerateTypeSigs
import SyntacticClass qualified as Syntactic
import Utils.Prelude hiding (readFile, writeFile)

-- | Everything the project analysis procedure needs from its execution context.
type Port m =
  ( MonadParallel m,
    Stages m,
    Warns m,
    FsOps m,
    ExecutesMigrations m,
    InfersQueryTypes m,
    ExplainsQuery m,
    LoadsIndexes m
  )

-- | Output of project analysis: the resolved generator model, any detected
-- sequential scans keyed by query name, and the current index catalog.
data Result = Result
  { project :: Gen.Input.Project,
    seqScanFindings :: [(Text, SeqScanFinding)],
    indexes :: [IndexInfo]
  }

-- | Input to project analysis.
data Params = Params
  { projectFile :: ProjectFile.ProjectFile
  }

-- * Private types

data QueryListed = QueryListed
  { name :: Name.Name,
    filePath :: Path,
    signatureFilePath :: Maybe Path
  }

-- * API

-- | Run the migrations, list and infer types for the project's queries,
-- check index coverage, and resolve the full generator model.
run :: (Port m) => Params -> m Result
run Params {projectFile} =
  stage "Analysing" 3 do
    migrationsLoaded <-
      stage "Migrations" 2 do
        migrationsListed <-
          listDir "migrations"
            & fmap (filter (\p -> Path.toExtensions p == ["sql"]))
            & fmap sort
            & fmap (fmap ("migrations" <>))

        let migrationsCount = length migrationsListed

        loaded <-
          stage "Loading" migrationsCount do
            MonadParallel.forM migrationsListed \migrationListed -> do
              stage (Path.toText migrationListed) 0 do
                migrationLoaded <- readFile migrationListed
                pure (migrationListed, migrationLoaded)

        stage "Executing" migrationsCount do
          for loaded \(migrationListed, migrationLoaded) -> do
            stage (Path.toText migrationListed) 0 do
              executeMigration migrationLoaded

        pure loaded

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
        name <- case Name.tryFromText (Path.toBasename queryPath) of
          Left err ->
            throwError
              ( Report
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

              maybeExplainFindings <-
                catchError
                  ( do
                      explainLines <- explainQuery nativeTemplate
                      let findings = SeqScanFinding.detectSeqScans explainLines
                      pure (Just (map (Name.inSnakeCase queryListed.name,) findings))
                  )
                  (\_ -> pure Nothing)

              let effectiveSeqScanFindings =
                    case maybeExplainFindings of
                      Just findings -> findings
                      Nothing ->
                        inferSeqScanFindingsFromSql nativeTemplate
                          & map (Name.inSnakeCase queryListed.name,)

              result :: Gen.Input.Result <-
                let rowsByCardinality cardinality =
                      case nonEmpty resultColumns of
                        Nothing ->
                          Nothing
                        Just columns ->
                          Just (Gen.Input.ResultRows cardinality columns)
                    classifyResult affectsRows = \case
                      Just rows ->
                        Gen.Input.RowsResult rows
                      Nothing ->
                        if affectsRows
                          then Gen.Input.RowsAffectedResult
                          else Gen.Input.VoidResult
                 in case SyntaxAnalyser.resolveText nativeTemplate of
                      Left err -> do
                        warn
                          ( Report
                              []
                              "Failed to detect result cardinality by AST. Defaulting to multi-row"
                              Nothing
                              [("error", err)]
                          )
                        pure $ classifyResult True (rowsByCardinality Gen.Input.MultipleResultRowsCardinality)
                      Right SyntaxAnalyser.QuerySyntaxAnalysis {affectsRows, resultRowAmount} ->
                        case resultRowAmount of
                          SyntaxAnalyser.SpecificRowAmount 0 ->
                            pure $ classifyResult affectsRows Nothing
                          SyntaxAnalyser.SpecificRowAmount 1 ->
                            pure $ classifyResult affectsRows (rowsByCardinality Gen.Input.SingleResultRowsCardinality)
                          SyntaxAnalyser.SpecificRowAmount _ ->
                            pure $ classifyResult affectsRows (rowsByCardinality Gen.Input.MultipleResultRowsCardinality)
                          SyntaxAnalyser.UpToRowAmount 0 ->
                            pure $ classifyResult affectsRows Nothing
                          SyntaxAnalyser.UpToRowAmount 1 ->
                            pure $ classifyResult affectsRows (rowsByCardinality Gen.Input.OptionalResultRowsCardinality)
                          SyntaxAnalyser.UpToRowAmount _ ->
                            pure $ classifyResult affectsRows (rowsByCardinality Gen.Input.MultipleResultRowsCardinality)
                          SyntaxAnalyser.AnyRowAmount ->
                            pure $ classifyResult affectsRows (rowsByCardinality Gen.Input.MultipleResultRowsCardinality)

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

              let sigPath = QuerySignature.signatureFilePath queryListed.filePath
                  inferredSig = QuerySignature.fromInferred interpretedParams result

              genSigResult <-
                GenerateQuerySigs.run
                  GenerateQuerySigs.Params
                    { sigPath = sigPath,
                      inferredSig = inferredSig,
                      inferredParams = interpretedParams,
                      inferredResult = result
                    }

              pure
                ( Gen.Input.Query
                    { name = Name.toGenName queryListed.name,
                      srcPath = Path.toText queryListed.filePath,
                      identity = False,
                      idempotent = genSigResult.idempotent,
                      params = genSigResult.params,
                      result = genSigResult.result,
                      fragments = SqlTemplate.toGenQueryFragments sqlTemplate
                    },
                  mentionedCustomTypes,
                  effectiveSeqScanFindings
                )

        let (queries0, customTypesDump, seqScanFindingsDump) = unzip3 mixedList
            dedupedCustomTypes =
              customTypesDump
                & concat
                & fmap (\x -> ((x.pgSchema, x.pgName), x))
                & Map.fromList
                & Map.elems
            (customTypes, queries) = CustomTypeOrdering.orderAndResolve dedupedCustomTypes queries0
            seqScanFindings = concat seqScanFindingsDump

        pure (queries, customTypes, seqScanFindings)

    refinedCustomTypes <-
      GenerateTypeSigs.run GenerateTypeSigs.Params {customTypes}
        <&> (.refinedCustomTypes)

    pure
      Result
        { project =
            Gen.Input.Project
              { space = Name.toGenName projectFile.space,
                name = Name.toGenName projectFile.name,
                version = projectFile.version,
                customTypes = refinedCustomTypes,
                queries = queries,
                migrations =
                  migrationsLoaded
                    & fmap
                      ( \(migrationPath, migrationSql) ->
                          Gen.Input.Migration
                            { name = Path.toBasename migrationPath,
                              sql = migrationSql
                            }
                      )
              },
          seqScanFindings = seqScanFindings,
          indexes = indexes
        }
  where
    loadQuerySql :: (Port m) => QueryListed -> m SqlTemplate.SqlTemplate
    loadQuerySql queryListed = do
      sql <- readFile queryListed.filePath
      case SqlTemplate.tryFromText sql of
        Left err ->
          throwError
            ( Report
                []
                "Failed to parse SQL template"
                (Just "Check the SQL syntax in the query file")
                [("file", Path.toText queryListed.filePath), ("error", to err)]
            )
        Right res -> pure res

    inferSeqScanFindingsFromSql :: Text -> [SeqScanFinding]
    inferSeqScanFindingsFromSql sql =
      case inferTableAndWhere sql of
        Nothing -> []
        Just (tbl, whereClause) ->
          let cols = SeqScanFinding.extractFilterColumns whereClause
           in if null cols
                then []
                else [SeqScanFinding tbl whereClause cols]
      where
        inferTableAndWhere :: Text -> Maybe (Text, Text)
        inferTableAndWhere sql = do
          let normalized =
                sql
                  & Text.lines
                  & filter (not . ("--" `Text.isPrefixOf`) . Text.stripStart)
                  & Text.unwords
              normalizedLower = Text.toLower normalized
              tokens = Text.words normalized
          guard (not (" join " `Text.isInfixOf` normalizedLower))
          table <- inferTableName tokens
          whereClause <- inferWhereClause tokens
          pure (table, whereClause)
          where
            inferTableName :: [Text] -> Maybe Text
            inferTableName tokens =
              findAfterKeyword "from"
                <|> findAfterKeyword "update"
                <|> ( do
                        i <- elemIndex "into" lowerTokens
                        prev <- lowerTokens !? (i - 1)
                        guard (prev == "insert")
                        token <- tokens !? (i + 1)
                        pure (normalizeTableToken token)
                    )
              where
                findAfterKeyword kw = do
                  i <- elemIndex kw lowerTokens
                  token <- tokens !? (i + 1)
                  pure (normalizeTableToken token)
                lowerTokens = map Text.toLower tokens

                normalizeTableToken :: Text -> Text
                normalizeTableToken token =
                  let cleaned =
                        token
                          & Text.dropWhile (\c -> c == '"' || c == '(')
                          & Text.takeWhile (\c -> c /= '"' && c /= ')' && c /= ';' && c /= ',')
                          & Text.splitOn "."
                   in case reverse cleaned of
                        [] -> ""
                        x : _ -> x

            inferWhereClause :: [Text] -> Maybe Text
            inferWhereClause tokens =
              let lowerTokens = map Text.toLower tokens
                  stopKeywords = Set.fromList ["group", "order", "limit", "returning", "union"]
               in do
                    i <- elemIndex "where" lowerTokens
                    let rest = drop (i + 1) tokens
                        restLower = drop (i + 1) lowerTokens
                        clauseTokens = map fst (takeWhile (\(_, lowerTok) -> lowerTok `Set.notMember` stopKeywords) (zip rest restLower))
                    guard (not (null clauseTokens))
                    pure (Text.unwords clauseTokens)
