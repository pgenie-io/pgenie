-- |
-- Analyzes existing indexes against observed query needs to recommend
-- redundant/excessive indexes to drop and missing ones to create, and
-- renders the recommendations as a migration.
module Logic.Domain.IndexOptimization
  ( IndexInfo (..),
    IndexAction (..),
    DropReason (..),
    optimizeIndexes,
    generateMigration,
    spec,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Test.Hspec
import Utils.Prelude

-- * Index domain types

data IndexInfo = IndexInfo
  { indexName :: Text,
    tableName :: Text,
    schemaName :: Text,
    columns :: [Text],
    isUnique :: Bool,
    isPrimary :: Bool,
    indexMethod :: Text,
    predicate :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | An action recommended by the index optimizer.
data IndexAction
  = -- | Drop an index that is unnecessary.
    DropIndexAction IndexInfo DropReason
  | -- | Create a new index to cover a missing access pattern.
    CreateIndexAction
      { tableName :: Text,
        columns :: [Text]
      }
  deriving stock (Eq, Show)

-- | Reason why an index should be dropped.
data DropReason
  = -- | This index's columns are a leading prefix of the superseding index's columns.
    PrefixRedundancyDropReason IndexInfo
  | -- | This index is an exact duplicate of another index.
    ExactDuplicateDropReason IndexInfo
  | -- | This composite index has trailing columns that are not needed by any query.
    --   The replacement columns are provided.
    ExcessiveCompositeDropReason [Text]
  | -- | This index is not used by any observed query need on the same table.
    UnusedByQueriesDropReason
  deriving stock (Eq, Show)

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
      -- Don't suggest unused-index removals for indexes that are already
      -- slated for removal by stronger structural reasons.
      droppedByStructure = Set.fromList (map dropIndexName (redundant <> filteredExcessive))
      unused = detectUnused indexes queryNeeds
      filteredUnused = filter (\a -> dropIndexName a `Set.notMember` droppedByStructure) unused
      -- Collect all create-index needs from seq-scan findings that are not
      -- already satisfied by existing indexes or by indexes that result from
      -- excessive-composite replacements.
      missing = detectMissing indexes (redundant <> filteredExcessive <> filteredUnused) queryNeeds
   in redundant <> filteredExcessive <> filteredUnused <> missing

dropIndexName :: IndexAction -> Text
dropIndexName (DropIndexAction idx _) = idx.indexName
dropIndexName (CreateIndexAction {}) = ""

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
              and
                [ other.indexName /= candidate.indexName,
                  other.tableName == candidate.tableName,
                  other.schemaName == candidate.schemaName,
                  other.indexMethod == candidate.indexMethod,
                  other.predicate == candidate.predicate
                ]
          )
          allIndexes
   in case findSuperseding candidate others of
        Nothing -> []
        Just reason -> [DropIndexAction candidate reason]

findSuperseding :: IndexInfo -> [IndexInfo] -> Maybe DropReason
findSuperseding candidate = go
  where
    go [] = Nothing
    go (other : rest)
      | candidate.columns
          == other.columns
          && (other.isPrimary || candidate.indexName > other.indexName) =
          Just (ExactDuplicateDropReason other)
      | isStrictPrefix candidate.columns other.columns =
          Just (PrefixRedundancyDropReason other)
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
                   in [DropIndexAction idx (ExcessiveCompositeDropReason replacement)]
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

-- * Unused index detection

-- | Detect indexes that do not cover any observed query need on their table.
--
-- This is only evaluated for tables that have at least one observed need,
-- to avoid proposing broad deletions when there is no workload signal.
detectUnused :: [IndexInfo] -> [(Text, [Text])] -> [IndexAction]
detectUnused indexes queryNeeds =
  let needsByTable = buildNeedsByTable queryNeeds
   in [ DropIndexAction idx UnusedByQueriesDropReason
      | idx <- indexes,
        not idx.isPrimary,
        not idx.isUnique,
        idx.indexMethod == "btree",
        idx.predicate == Nothing,
        Just needs <- [Map.lookup idx.tableName needsByTable],
        let needsList = Set.toList needs,
        not (null needsList),
        not (any (\needCols -> needCols `isPrefixOf` idx.columns) needsList)
      ]

-- * Missing index detection

-- | Suggest CREATE INDEX actions for tables that have seq-scan findings
-- not already covered by an existing or replacement index.
detectMissing :: [IndexInfo] -> [IndexAction] -> [(Text, [Text])] -> [IndexAction]
detectMissing indexes actions queryNeeds =
  let -- Effective indexes after applying drop/create from redundant & excessive.
      droppedNames = Set.fromList [idx.indexName | DropIndexAction idx _ <- actions]
      survivingIndexes = filter (\idx -> idx.indexName `Set.notMember` droppedNames) indexes
      -- Replacement indexes from excessive-composite actions.
      replacements = [(idx.tableName, cols) | DropIndexAction idx (ExcessiveCompositeDropReason cols) <- actions]
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
   in [ CreateIndexAction tbl cols
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
  let drops = [a | a@(DropIndexAction _ _) <- actions]
      creates = [a | a@(CreateIndexAction {}) <- actions]
      dropStmts = concatMap dropStatement drops
      createStmts = concatMap createStatement creates
      replacementCreates =
        [ createStatement (CreateIndexAction idx.tableName cols)
        | DropIndexAction idx (ExcessiveCompositeDropReason cols) <- actions
        ]
      allLines =
        ["-- Auto-generated migration to optimize indexes", ""]
          <> (if null drops then [] else ["-- Drop redundant/excessive indexes"] <> dropStmts)
          <> (if null replacementCreates then [] else ["-- Create replacement indexes for narrowed composites"] <> concat replacementCreates)
          <> (if null creates then [] else ["-- Create missing indexes"] <> createStmts)
   in Text.unlines allLines
  where
    dropStatement :: IndexAction -> [Text]
    dropStatement (DropIndexAction idx reason) =
      [ "-- " <> reasonComment idx reason,
        "DROP INDEX " <> quoteIdent idx.schemaName <> "." <> quoteIdent idx.indexName <> ";",
        ""
      ]
    dropStatement _ = []

    createStatement :: IndexAction -> [Text]
    createStatement (CreateIndexAction tbl cols) =
      [ "CREATE INDEX ON " <> tbl <> " (" <> Text.intercalate ", " cols <> ");",
        ""
      ]
    createStatement _ = []

    reasonComment :: IndexInfo -> DropReason -> Text
    reasonComment idx reason = case reason of
      ExactDuplicateDropReason other ->
        idx.indexName <> " is an exact duplicate of " <> other.indexName
      PrefixRedundancyDropReason other ->
        idx.indexName <> " is a prefix of " <> other.indexName
      ExcessiveCompositeDropReason replacement ->
        idx.indexName
          <> " on ("
          <> Text.intercalate ", " idx.columns
          <> ") is excessive; replacing with ("
          <> Text.intercalate ", " replacement
          <> ")"
      UnusedByQueriesDropReason ->
        idx.indexName
          <> " on ("
          <> Text.intercalate ", " idx.columns
          <> ") is not used by observed query needs"

    quoteIdent :: Text -> Text
    quoteIdent t = "\"" <> Text.replace "\"" "\"\"" t <> "\""

spec :: Spec
spec = do
  describe "optimizeIndexes" do
    describe "redundant index detection" do
      it "returns empty list when no indexes are provided" do
        optimizeIndexes [] [] `shouldBe` []

      it "returns empty list for a single index" do
        let indexes = [mkIndex "idx1" "users" ["id"] False False]
        optimizeIndexes indexes [] `shouldBe` []

      it "detects exact duplicate indexes" do
        let idx1 = mkIndex "idx1" "users" ["email"] False False
            idx2 = mkIndex "idx2" "users" ["email"] False False
            result = optimizeIndexes [idx1, idx2] []
        length result `shouldBe` 1
        case result of
          [DropIndexAction idx (ExactDuplicateDropReason other)] -> do
            idx.indexName `shouldBe` "idx2"
            other.indexName `shouldBe` "idx1"
          _ -> expectationFailure "Expected single DropIndexAction with ExactDuplicateDropReason"

      it "detects prefix redundancy" do
        let idx1 = mkIndex "idx_a" "users" ["email"] False False
            idx2 = mkIndex "idx_ab" "users" ["email", "name"] False False
            result = optimizeIndexes [idx1, idx2] []
        length result `shouldBe` 1
        case result of
          [DropIndexAction idx (PrefixRedundancyDropReason other)] -> do
            idx.indexName `shouldBe` "idx_a"
            other.indexName `shouldBe` "idx_ab"
          _ -> expectationFailure "Expected single DropIndexAction with PrefixRedundancyDropReason"

      it "does not flag indexes on different tables" do
        let idx1 = mkIndex "idx1" "users" ["email"] False False
            idx2 = mkIndex "idx2" "orders" ["email"] False False
        optimizeIndexes [idx1, idx2] [] `shouldBe` []

      it "does not flag indexes with different methods" do
        let idx1 = mkIndexWithMethod "idx1" "users" ["data"] "btree" False False
            idx2 = mkIndexWithMethod "idx2" "users" ["data"] "gin" False False
        optimizeIndexes [idx1, idx2] [] `shouldBe` []

      it "does not flag primary key indexes as redundant" do
        let idx1 = mkIndex "pk_users" "users" ["id"] False True
            idx2 = mkIndex "idx_users_id" "users" ["id"] False False
            result = optimizeIndexes [idx1, idx2] []
        length result `shouldBe` 1
        case result of
          [DropIndexAction idx _] -> idx.indexName `shouldBe` "idx_users_id"
          _ -> expectationFailure "Expected single DropIndexAction"

      it "does not flag indexes with different predicates" do
        let idx1 = mkIndexWithPredicate "idx1" "users" ["email"] Nothing
            idx2 = mkIndexWithPredicate "idx2" "users" ["email"] (Just "(active = true)")
        optimizeIndexes [idx1, idx2] [] `shouldBe` []

      it "detects prefix redundancy with matching predicates" do
        let idx1 = mkIndexWithPredicate "idx1" "users" ["email"] (Just "(active = true)")
            idx2 = mkIndexWithPredicate "idx2" "users" ["email", "name"] (Just "(active = true)")
            result = optimizeIndexes [idx1, idx2] []
        length result `shouldBe` 1
        case result of
          [DropIndexAction idx _] -> idx.indexName `shouldBe` "idx1"
          _ -> expectationFailure "Expected single DropIndexAction"

      it "does not flag non-prefix column overlap" do
        let idx1 = mkIndex "idx1" "users" ["name"] False False
            idx2 = mkIndex "idx2" "users" ["email", "name"] False False
        optimizeIndexes [idx1, idx2] [] `shouldBe` []

      it "handles multiple redundancies" do
        let idxA = mkIndex "idx_a" "users" ["email"] False False
            idxAB = mkIndex "idx_ab" "users" ["email", "name"] False False
            idxABC = mkIndex "idx_abc" "users" ["email", "name", "age"] False False
            result = optimizeIndexes [idxA, idxAB, idxABC] []
            drops = [a | a@(DropIndexAction _ _) <- result]
        length drops `shouldBe` 2

    describe "excessive composite index detection" do
      it "narrows composite index when only a prefix is queried" do
        let idx = mkIndex "idx_name_format" "album" ["name", "format"] False False
            queryNeeds = [("album", ["name"])]
            result = optimizeIndexes [idx] queryNeeds
            drops = [a | a@(DropIndexAction _ (ExcessiveCompositeDropReason _)) <- result]
        length drops `shouldBe` 1
        case drops of
          [DropIndexAction _ (ExcessiveCompositeDropReason replacement)] ->
            replacement `shouldBe` ["name"]
          _ -> expectationFailure "Expected ExcessiveCompositeDropReason"

      it "does not narrow unique indexes" do
        let idx = mkIndex "idx_name_format" "album" ["name", "format"] True False
            queryNeeds = [("album", ["name"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

      it "does not narrow primary key indexes" do
        let idx = mkIndex "pk_ab" "album_artist" ["album", "artist"] False True
            queryNeeds = [("album_artist", ["album"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

      it "keeps all columns when all are needed" do
        let idx = mkIndex "idx_ab" "users" ["email", "name"] False False
            queryNeeds = [("users", ["email", "name"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

      it "uses the maximum prefix across multiple queries" do
        let idx = mkIndex "idx_abc" "users" ["email", "name", "age"] False False
            queryNeeds =
              [ ("users", ["email"]),
                ("users", ["email", "name"])
              ]
            result = optimizeIndexes [idx] queryNeeds
            drops = [a | a@(DropIndexAction _ (ExcessiveCompositeDropReason _)) <- result]
        length drops `shouldBe` 1
        case drops of
          [DropIndexAction _ (ExcessiveCompositeDropReason replacement)] ->
            replacement `shouldBe` ["email", "name"]
          _ -> expectationFailure "Expected ExcessiveCompositeDropReason"

      it "does not narrow single-column indexes" do
        let idx = mkIndex "idx_email" "users" ["email"] False False
            queryNeeds = [("users", ["email"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

      it "ignores non-btree indexes for narrowing" do
        let idx = mkIndexWithMethod "idx_data" "users" ["data", "extra"] "gin" False False
            queryNeeds = [("users", ["data"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

    describe "missing index detection" do
      it "suggests a missing index" do
        let queryNeeds = [("album", ["format"])]
            result = optimizeIndexes [] queryNeeds
        result `shouldBe` [CreateIndexAction "album" ["format"]]

      it "drops an index unused by observed query needs" do
        let idx = mkIndex "idx_recording" "album" ["recording"] False False
            queryNeeds = [("album", ["format"])]
            result = optimizeIndexes [idx] queryNeeds
            drops = [a | a@(DropIndexAction _ UnusedByQueriesDropReason) <- result]
        drops `shouldSatisfy` (not . null)

      it "does not suggest an index that already exists" do
        let idx = mkIndex "idx_format" "album" ["format"] False False
            queryNeeds = [("album", ["format"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

      it "recognizes prefix coverage from a wider index" do
        let idx = mkIndex "idx_format_name" "album" ["format", "name"] False False
            queryNeeds = [("album", ["format"])]
            result = optimizeIndexes [idx] queryNeeds
            creates = [a | a@(CreateIndexAction {}) <- result]
        creates `shouldBe` []

      it "suggests index when no existing index covers the columns" do
        let idx = mkIndex "idx_name" "album" ["name"] False False
            queryNeeds = [("album", ["format"])]
            result = optimizeIndexes [idx] queryNeeds
            creates = [a | a@(CreateIndexAction {}) <- result]
            drops = [a | a@(DropIndexAction _ UnusedByQueriesDropReason) <- result]
        creates `shouldBe` [CreateIndexAction "album" ["format"]]
        drops `shouldSatisfy` (not . null)

      it "does not suggest duplicate creates for the same columns" do
        let queryNeeds = [("album", ["format"]), ("album", ["format"])]
            result = optimizeIndexes [] queryNeeds
        result `shouldBe` [CreateIndexAction "album" ["format"]]

      it "skips empty column lists from query needs" do
        let queryNeeds = [("album", [])]
        optimizeIndexes [] queryNeeds `shouldBe` []

    describe "combined optimization" do
      it "handles redundant drop + missing create together" do
        let idx1 = mkIndex "idx1" "album" ["name"] False False
            idx2 = mkIndex "idx2" "album" ["name", "format"] False False
            queryNeeds = [("album", ["released"])]
            result = optimizeIndexes [idx1, idx2] queryNeeds
            drops = [a | a@(DropIndexAction _ _) <- result]
            creates = [a | a@(CreateIndexAction {}) <- result]
        length drops `shouldSatisfy` (>= 1)
        length creates `shouldBe` 1
        case creates of
          [CreateIndexAction tbl cols] -> do
            tbl `shouldBe` "album"
            cols `shouldBe` ["released"]
          _ -> expectationFailure "Expected single CreateIndexAction"

      it "accounts for excessive-composite replacement when checking missing" do
        let idx = mkIndex "idx_nf" "album" ["name", "format"] False False
            queryNeeds = [("album", ["name"])]
            result = optimizeIndexes [idx] queryNeeds
            creates = [a | a@(CreateIndexAction {}) <- result]
        creates `shouldBe` []

      it "handles multiple tables independently" do
        let idx1 = mkIndex "idx_a1" "users" ["email"] False False
            idx2 = mkIndex "idx_a2" "users" ["email"] False False
            idx3 = mkIndex "idx_b1" "orders" ["status", "date"] False False
            queryNeeds = [("orders", ["status"]), ("users", ["name"])]
            result = optimizeIndexes [idx1, idx2, idx3] queryNeeds
            drops = [a | a@(DropIndexAction _ _) <- result]
            creates = [a | a@(CreateIndexAction {}) <- result]
        length drops `shouldSatisfy` (>= 2)
        creates `shouldSatisfy` any (\case CreateIndexAction t c -> t == "users" && c == ["name"]; _ -> False)

    describe "composite narrowing edge cases" do
      it "only considers contiguous leading prefix for narrowing" do
        let idx = mkIndex "idx_abc" "t" ["a", "b", "c"] False False
            queryNeeds = [("t", ["a", "c"])]
            result = optimizeIndexes [idx] queryNeeds
            drops = [a | a@(DropIndexAction _ (ExcessiveCompositeDropReason _)) <- result]
        case drops of
          [DropIndexAction _ (ExcessiveCompositeDropReason replacement)] ->
            replacement `shouldBe` ["a"]
          _ -> expectationFailure "Expected ExcessiveCompositeDropReason with [a]"

      it "does not narrow if query uses no leading columns" do
        let idx = mkIndex "idx_ab" "t" ["a", "b"] False False
            queryNeeds = [("t", ["b"])]
            result = optimizeIndexes [idx] queryNeeds
            excessiveDrops = [a | a@(DropIndexAction _ (ExcessiveCompositeDropReason _)) <- result]
        excessiveDrops `shouldBe` []

      it "handles disjoint query sets requiring separate indexes" do
        let idx = mkIndex "idx_en" "users" ["email", "name"] False False
            queryNeeds = [("users", ["email"]), ("users", ["name"])]
            result = optimizeIndexes [idx] queryNeeds
            creates = [a | a@(CreateIndexAction {}) <- result]
        creates `shouldSatisfy` any (\case CreateIndexAction _ c -> c == ["name"]; _ -> False)

      it "does not narrow when all columns are needed across queries" do
        let idx = mkIndex "idx_abc" "t" ["a", "b", "c"] False False
            queryNeeds = [("t", ["a"]), ("t", ["a", "b"]), ("t", ["a", "b", "c"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

      it "narrows to 2 when one query needs [a] and another needs [a,b] out of [a,b,c]" do
        let idx = mkIndex "idx_abc" "t" ["a", "b", "c"] False False
            queryNeeds = [("t", ["a"]), ("t", ["a", "b"])]
            result = optimizeIndexes [idx] queryNeeds
            drops = [a | a@(DropIndexAction _ (ExcessiveCompositeDropReason _)) <- result]
        case drops of
          [DropIndexAction _ (ExcessiveCompositeDropReason replacement)] ->
            replacement `shouldBe` ["a", "b"]
          _ -> expectationFailure "Expected ExcessiveCompositeDropReason with [a, b]"

  describe "generateMigration" do
    it "generates DROP and CREATE statements in a single migration" do
      let actions =
            [ DropIndexAction (mkIndex "idx_dup" "users" ["email"] False False) (ExactDuplicateDropReason (mkIndex "idx_orig" "users" ["email"] False False)),
              CreateIndexAction "album" ["format"]
            ]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "DROP INDEX" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "CREATE INDEX" `Text.isInfixOf` t

    it "includes replacement indexes for excessive composites" do
      let idx = mkIndex "idx_nf" "album" ["name", "format"] False False
          actions = [DropIndexAction idx (ExcessiveCompositeDropReason ["name"])]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "DROP INDEX" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "CREATE INDEX ON album (name)" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "replacing with" `Text.isInfixOf` t

    it "generates empty migration for no actions" do
      generateMigration [] `shouldSatisfy` \t -> not ("DROP" `Text.isInfixOf` t || "CREATE" `Text.isInfixOf` t)

    it "handles prefix-redundancy drop with comment" do
      let idx = mkIndex "idx_a" "users" ["email"] False False
          superseder = mkIndex "idx_ab" "users" ["email", "name"] False False
          actions = [DropIndexAction idx (PrefixRedundancyDropReason superseder)]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "DROP INDEX" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "is a prefix of" `Text.isInfixOf` t

    it "handles unused-by-queries drop with comment" do
      let idx = mkIndex "idx_stale" "album" ["format"] False False
          actions = [DropIndexAction idx UnusedByQueriesDropReason]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "DROP INDEX" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "not used by observed query needs" `Text.isInfixOf` t

    it "quotes schema and index names in DROP statements" do
      let idx = mkIndex "my-index" "album" ["name"] False False
          actions = [DropIndexAction idx (ExactDuplicateDropReason idx)]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "\"public\".\"my-index\"" `Text.isInfixOf` t

    it "generates all sections in correct order" do
      let redIdx = mkIndex "idx_red" "users" ["email"] False False
          compIdx = mkIndex "idx_comp" "users" ["name", "age"] False False
          actions =
            [ DropIndexAction redIdx (ExactDuplicateDropReason (mkIndex "idx_orig" "users" ["email"] False False)),
              DropIndexAction compIdx (ExcessiveCompositeDropReason ["name"]),
              CreateIndexAction "orders" ["status"]
            ]
          migration = generateMigration actions
          findPos needle haystack = Text.length (fst (Text.breakOn needle haystack))
          dropPos = findPos "Drop redundant" migration
          replPos = findPos "replacement" migration
          createPos = findPos "missing" migration
      dropPos `shouldSatisfy` (< replPos)
      replPos `shouldSatisfy` (< createPos)

mkIndex :: Text -> Text -> [Text] -> Bool -> Bool -> IndexInfo
mkIndex name table cols isUniq isPrim =
  IndexInfo
    { indexName = name,
      tableName = table,
      schemaName = "public",
      columns = cols,
      isUnique = isUniq,
      isPrimary = isPrim,
      indexMethod = "btree",
      predicate = Nothing
    }

mkIndexWithMethod :: Text -> Text -> [Text] -> Text -> Bool -> Bool -> IndexInfo
mkIndexWithMethod name table cols method isUniq isPrim =
  (mkIndex name table cols isUniq isPrim) {indexMethod = method}

mkIndexWithPredicate :: Text -> Text -> [Text] -> Maybe Text -> IndexInfo
mkIndexWithPredicate name table cols predicate =
  (mkIndex name table cols False False) {predicate}
