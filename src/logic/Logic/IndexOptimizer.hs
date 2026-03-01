module Logic.IndexOptimizer
  ( optimizeIndexes,
    generateMigration,
  )
where

import Base.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Logic.Algebra

-- | Analyze existing indexes against query needs and produce a list of
-- recommended actions: indexes to drop (redundant / excessive) and
-- indexes to create (missing).
--
-- @queryNeeds@ is a list of @(tableName, columnsUsedInFilter)@ pairs
-- gathered from sequential-scan findings across all queries.  Multiple
-- queries may contribute different column sets for the same table; they
-- are all taken into account.
optimizeIndexes :: [IndexInfo] -> [(Text, [Text])] -> [IndexAction]
optimizeIndexes indexes queryNeeds =
  let redundant = detectRedundant indexes
      excessive = detectExcessive indexes queryNeeds
      -- Don't suggest excessive-composite changes for indexes that are already
      -- slated for removal as redundant.
      redundantNames = Set.fromList (map dropIndexName redundant)
      filteredExcessive = filter (\a -> dropIndexName a `Set.notMember` redundantNames) excessive
      -- Collect all create-index needs from seq-scan findings that are not
      -- already satisfied by existing indexes or by indexes that result from
      -- excessive-composite replacements.
      missing = detectMissing indexes (redundant <> filteredExcessive) queryNeeds
   in redundant <> filteredExcessive <> missing

dropIndexName :: IndexAction -> Text
dropIndexName (DropIndex idx _) = idx.indexName
dropIndexName (CreateIndex {}) = ""

-- * Redundant index detection

-- | Detect indexes that are exact duplicates or leading prefixes of other indexes.
-- Primary key indexes are never flagged as redundant.
detectRedundant :: [IndexInfo] -> [IndexAction]
detectRedundant indexes =
  let nonPrimary = filter (not . (.isPrimary)) indexes
   in concatMap (findRedundancies indexes) nonPrimary

findRedundancies :: [IndexInfo] -> IndexInfo -> [IndexAction]
findRedundancies allIndexes candidate =
  let others =
        filter
          ( \other ->
              other.indexName /= candidate.indexName
                && other.tableName == candidate.tableName
                && other.schemaName == candidate.schemaName
                && other.indexMethod == candidate.indexMethod
                && other.predicate == candidate.predicate
          )
          allIndexes
   in case findSuperseding candidate others of
        Nothing -> []
        Just reason -> [DropIndex candidate reason]

findSuperseding :: IndexInfo -> [IndexInfo] -> Maybe DropReason
findSuperseding candidate = go
  where
    go [] = Nothing
    go (other : rest)
      | candidate.columns == other.columns
          && (other.isPrimary || candidate.indexName > other.indexName) =
          Just (ExactDuplicate other)
      | isStrictPrefix candidate.columns other.columns =
          Just (PrefixRedundancy other)
      | otherwise = go rest

-- | Check if the first list is a strict prefix of the second.
isStrictPrefix :: (Eq a) => [a] -> [a] -> Bool
isStrictPrefix [] (_ : _) = True
isStrictPrefix _ [] = False
isStrictPrefix (x : xs) (y : ys) = x == y && isStrictPrefix xs ys

-- * Excessive composite index detection

-- | Detect composite indexes whose trailing columns are never used by any query.
--
-- For each non-unique, non-primary btree index with more than one column,
-- check whether all queries that touch the table only use a leading
-- subset of those columns.  If so, the index can be narrowed.
--
-- Unique indexes are excluded because removing trailing columns could
-- change the uniqueness constraint semantics.
detectExcessive :: [IndexInfo] -> [(Text, [Text])] -> [IndexAction]
detectExcessive indexes queryNeeds =
  let needsByTable = buildNeedsByTable queryNeeds
   in concatMap (checkExcessive needsByTable) indexes

checkExcessive :: Map Text (Set [Text]) -> IndexInfo -> [IndexAction]
checkExcessive needsByTable idx
  | length idx.columns <= 1 = []
  | idx.isUnique = []
  | idx.isPrimary = []
  | idx.indexMethod /= "btree" = []
  | otherwise =
      case Map.lookup idx.tableName needsByTable of
        Nothing -> []
        Just columnSets ->
          let neededLen = maxPrefixNeeded idx.columns columnSets
           in if neededLen > 0 && neededLen < length idx.columns
                then
                  let replacement = take neededLen idx.columns
                   in [DropIndex idx (ExcessiveComposite replacement)]
                else []

-- | Given the columns of an index and a set of column lists from queries,
-- determine how many leading columns of the index are actually needed.
--
-- A query's column list "needs" N leading columns if the query columns
-- are exactly covered by the first N columns of the index (i.e. the
-- query columns are a subset of the index prefix that covers them).
maxPrefixNeeded :: [Text] -> Set [Text] -> Int
maxPrefixNeeded indexCols columnSets =
  Set.foldl' (\acc queryCols -> max acc (prefixNeeded indexCols queryCols)) 0 columnSets

-- | How many leading columns of the index does this query need?
-- Returns 0 if the query doesn't use the first column of the index.
-- Only counts contiguous leading prefix columns, since btree indexes
-- can only be used left-to-right without gaps.
prefixNeeded :: [Text] -> [Text] -> Int
prefixNeeded indexCols queryCols =
  let querySet = Set.fromList queryCols
   in go 0 indexCols querySet
  where
    go depth [] _ = depth
    go depth (ic : ics) qs
      | Set.member ic qs = go (depth + 1) ics qs
      | otherwise = depth

-- * Missing index detection

-- | Suggest CREATE INDEX actions for tables that have seq-scan findings
-- not already covered by an existing or replacement index.
detectMissing :: [IndexInfo] -> [IndexAction] -> [(Text, [Text])] -> [IndexAction]
detectMissing indexes actions queryNeeds =
  let -- Effective indexes after applying drop/create from redundant & excessive.
      droppedNames = Set.fromList [idx.indexName | DropIndex idx _ <- actions]
      survivingIndexes = filter (\idx -> idx.indexName `Set.notMember` droppedNames) indexes
      -- Replacement indexes from excessive-composite actions.
      replacements = [(idx.tableName, cols) | DropIndex idx (ExcessiveComposite cols) <- actions]
      -- Build effective coverage: for each table, set of column-lists covered.
      coverageMap = buildCoverageMap survivingIndexes replacements
      -- Deduplicate query needs per table.
      needsByTable = buildNeedsByTable queryNeeds
      -- For each table's needed column-set, check if it's covered.
      needed = Map.toList needsByTable >>= uncurry (findUncovered coverageMap)
   in nub needed

buildNeedsByTable :: [(Text, [Text])] -> Map Text (Set [Text])
buildNeedsByTable =
  foldl'
    (\acc (tbl, cols) -> Map.insertWith Set.union tbl (Set.singleton cols) acc)
    Map.empty

buildCoverageMap :: [IndexInfo] -> [(Text, [Text])] -> Map Text [[Text]]
buildCoverageMap indexes replacements =
  let fromIndexes =
        foldl'
          (\acc idx -> Map.insertWith (<>) idx.tableName [idx.columns] acc)
          Map.empty
          indexes
      fromReplacements =
        foldl'
          (\acc (tbl, cols) -> Map.insertWith (<>) tbl [cols] acc)
          Map.empty
          replacements
   in Map.unionWith (<>) fromIndexes fromReplacements

findUncovered :: Map Text [[Text]] -> Text -> Set [Text] -> [IndexAction]
findUncovered coverageMap tbl columnSets =
  let covered = fromMaybe [] (Map.lookup tbl coverageMap)
   in [ CreateIndex tbl cols
      | cols <- Set.toList columnSets,
        not (null cols),
        not (anyCoversCols covered cols)
      ]

-- | Does any existing index cover the given query columns?
-- An index covers a set of query columns if those columns form a prefix
-- of (or are equal to) the index's column list.
anyCoversCols :: [[Text]] -> [Text] -> Bool
anyCoversCols indexColLists queryCols =
  any (\ic -> queryCols `isPrefixOf` ic) indexColLists

-- * Migration generation

-- | Generate a SQL migration combining all index actions.
generateMigration :: [IndexAction] -> Text
generateMigration actions =
  let drops = [a | a@(DropIndex _ _) <- actions]
      creates = [a | a@(CreateIndex {}) <- actions]
      dropStmts = concatMap dropStatement drops
      createStmts = concatMap createStatement creates
      replacementCreates =
        [ createStatement (CreateIndex idx.tableName cols)
        | DropIndex idx (ExcessiveComposite cols) <- actions
        ]
      allLines =
        ["-- Auto-generated migration to optimize indexes", ""]
          <> (if null drops then [] else ["-- Drop redundant/excessive indexes"] <> dropStmts)
          <> (if null replacementCreates then [] else ["-- Create replacement indexes for narrowed composites"] <> concat replacementCreates)
          <> (if null creates then [] else ["-- Create missing indexes"] <> createStmts)
   in Text.unlines allLines
  where
    dropStatement :: IndexAction -> [Text]
    dropStatement (DropIndex idx reason) =
      [ "-- " <> reasonComment idx reason,
        "DROP INDEX " <> quoteIdent idx.schemaName <> "." <> quoteIdent idx.indexName <> ";",
        ""
      ]
    dropStatement _ = []

    createStatement :: IndexAction -> [Text]
    createStatement (CreateIndex tbl cols) =
      [ "CREATE INDEX ON " <> tbl <> " (" <> Text.intercalate ", " cols <> ");",
        ""
      ]
    createStatement _ = []

    reasonComment :: IndexInfo -> DropReason -> Text
    reasonComment idx reason = case reason of
      ExactDuplicate other ->
        idx.indexName <> " is an exact duplicate of " <> other.indexName
      PrefixRedundancy other ->
        idx.indexName <> " is a prefix of " <> other.indexName
      ExcessiveComposite replacement ->
        idx.indexName
          <> " on ("
          <> Text.intercalate ", " idx.columns
          <> ") is excessive; replacing with ("
          <> Text.intercalate ", " replacement
          <> ")"

    quoteIdent :: Text -> Text
    quoteIdent t = "\"" <> Text.replace "\"" "\"\"" t <> "\""
