module Logic.Workflows.AnalyseProject where

import AlgebraicPath qualified as Path
import Control.Monad.Parallel qualified as MonadParallel
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Logic.Features.Fs (FsOps (..))
import Logic.Features.IndexOptimizer (IndexInfo (..), LoadsIndexes (..))
import Logic.Features.Migrations (ExecutesMigrations (..))
import Logic.Features.Name qualified as Name
import Logic.Features.ProjectFile qualified as ProjectFile
import Logic.Features.QueryAnalysis (InferredParam (..), InferredQueryTypes (..), InfersQueryTypes (..))
import Logic.Features.Report (Report (..), Warns (..))
import Logic.Features.SeqScanDetector (ExplainsQuery (..), SeqScanFinding (..))
import Logic.Features.SeqScanDetector qualified as SeqScanDetector
import Logic.Features.SignatureFile qualified as SignatureFile
import Logic.Features.SqlTemplate qualified as SqlTemplate
import Logic.Features.Staging (Stages (..))
import Logic.Features.SyntaxAnalyser qualified as SyntaxAnalyser
import Logic.Workflows.GenerateQuerySigs qualified as GenerateQuerySigs
import Logic.Workflows.GenerateTypeSigs qualified as GenerateTypeSigs
import PGenieGen.Model.Input qualified as Gen.Input
import SyntacticClass qualified as Syntactic
import Utils.Prelude hiding (readFile, writeFile)

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

data Result = Result
  { project :: Gen.Input.Project,
    seqScanFindings :: [(Text, SeqScanFinding)],
    indexes :: [IndexInfo]
  }

-- * Private types

data QueryListed = QueryListed
  { name :: Name.Name,
    filePath :: Path,
    signatureFilePath :: Maybe Path
  }

-- * API

run :: (Port m) => ProjectFile.ProjectFile -> m Result
run projectFile =
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

              querySeqScanFindings <-
                catchError
                  ( do
                      explainLines <- explainQuery nativeTemplate
                      let findings = SeqScanDetector.detectSeqScans explainLines
                      pure (map (Name.inSnakeCase queryListed.name,) findings)
                  )
                  (\_ -> pure [])

              let fallbackSeqScanFindings =
                    inferSeqScanFindingsFromSql nativeTemplate
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
                          ( Report
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

              let sigPath = SignatureFile.signatureFilePath queryListed.filePath
                  inferredSig = SignatureFile.fromInferred interpretedParams result

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
                      srcPath = queryListed.filePath,
                      idempotent = genSigResult.idempotent,
                      params = genSigResult.params,
                      result = genSigResult.result,
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

    refinedCustomTypes <- GenerateTypeSigs.run customTypes

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
          let cols = SeqScanDetector.extractFilterColumns whereClause
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
