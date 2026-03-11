module IndexOptimizerSpec (spec) where

import Base.Prelude
import Data.Text qualified as Text
import Logic.Algebra
import Logic.IndexOptimizer
import Test.Hspec

spec :: Spec
spec = do
  describe "optimizeIndexes" do
    -- Redundant index detection (carried over from RedundantIndexDetector)
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
          [DropIndex idx (ExactDuplicate other)] -> do
            idx.indexName `shouldBe` "idx2"
            other.indexName `shouldBe` "idx1"
          _ -> expectationFailure "Expected single DropIndex with ExactDuplicate"

      it "detects prefix redundancy" do
        let idx1 = mkIndex "idx_a" "users" ["email"] False False
            idx2 = mkIndex "idx_ab" "users" ["email", "name"] False False
            result = optimizeIndexes [idx1, idx2] []
        length result `shouldBe` 1
        case result of
          [DropIndex idx (PrefixRedundancy other)] -> do
            idx.indexName `shouldBe` "idx_a"
            other.indexName `shouldBe` "idx_ab"
          _ -> expectationFailure "Expected single DropIndex with PrefixRedundancy"

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
          [DropIndex idx _] -> idx.indexName `shouldBe` "idx_users_id"
          _ -> expectationFailure "Expected single DropIndex"

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
          [DropIndex idx _] -> idx.indexName `shouldBe` "idx1"
          _ -> expectationFailure "Expected single DropIndex"

      it "does not flag non-prefix column overlap" do
        let idx1 = mkIndex "idx1" "users" ["name"] False False
            idx2 = mkIndex "idx2" "users" ["email", "name"] False False
        optimizeIndexes [idx1, idx2] [] `shouldBe` []

      it "handles multiple redundancies" do
        let idxA = mkIndex "idx_a" "users" ["email"] False False
            idxAB = mkIndex "idx_ab" "users" ["email", "name"] False False
            idxABC = mkIndex "idx_abc" "users" ["email", "name", "age"] False False
            result = optimizeIndexes [idxA, idxAB, idxABC] []
            drops = [a | a@(DropIndex _ _) <- result]
        length drops `shouldBe` 2

    -- Excessive composite index detection
    describe "excessive composite index detection" do
      it "narrows composite index when only a prefix is queried" do
        let idx = mkIndex "idx_name_format" "album" ["name", "format"] False False
            queryNeeds = [("album", ["name"])]
            result = optimizeIndexes [idx] queryNeeds
            drops = [a | a@(DropIndex _ (ExcessiveComposite _)) <- result]
        length drops `shouldBe` 1
        case drops of
          [DropIndex _ (ExcessiveComposite replacement)] ->
            replacement `shouldBe` ["name"]
          _ -> expectationFailure "Expected ExcessiveComposite"

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
            drops = [a | a@(DropIndex _ (ExcessiveComposite _)) <- result]
        length drops `shouldBe` 1
        case drops of
          [DropIndex _ (ExcessiveComposite replacement)] ->
            replacement `shouldBe` ["email", "name"]
          _ -> expectationFailure "Expected ExcessiveComposite"

      it "does not narrow single-column indexes" do
        let idx = mkIndex "idx_email" "users" ["email"] False False
            queryNeeds = [("users", ["email"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

      it "ignores non-btree indexes for narrowing" do
        let idx = mkIndexWithMethod "idx_data" "users" ["data", "extra"] "gin" False False
            queryNeeds = [("users", ["data"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

    -- Missing index detection
    describe "missing index detection" do
      it "suggests a missing index" do
        let queryNeeds = [("album", ["format"])]
            result = optimizeIndexes [] queryNeeds
        result `shouldBe` [CreateIndex "album" ["format"]]

      it "drops an index unused by observed query needs" do
        let idx = mkIndex "idx_recording" "album" ["recording"] False False
            queryNeeds = [("album", ["format"])]
            result = optimizeIndexes [idx] queryNeeds
            drops = [a | a@(DropIndex _ UnusedByQueries) <- result]
        drops `shouldSatisfy` (not . null)

      it "does not suggest an index that already exists" do
        let idx = mkIndex "idx_format" "album" ["format"] False False
            queryNeeds = [("album", ["format"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

      it "recognizes prefix coverage from a wider index" do
        let idx = mkIndex "idx_format_name" "album" ["format", "name"] False False
            queryNeeds = [("album", ["format"])]
            result = optimizeIndexes [idx] queryNeeds
            creates = [a | a@(CreateIndex {}) <- result]
        -- The wider index covers "format" — no new index needed.
        -- But the excessive detector correctly flags it for narrowing.
        creates `shouldBe` []

      it "suggests index when no existing index covers the columns" do
        let idx = mkIndex "idx_name" "album" ["name"] False False
            queryNeeds = [("album", ["format"])]
            result = optimizeIndexes [idx] queryNeeds
            creates = [a | a@(CreateIndex {}) <- result]
            drops = [a | a@(DropIndex _ UnusedByQueries) <- result]
        creates `shouldBe` [CreateIndex "album" ["format"]]
        drops `shouldSatisfy` (not . null)

      it "does not suggest duplicate creates for the same columns" do
        let queryNeeds = [("album", ["format"]), ("album", ["format"])]
            result = optimizeIndexes [] queryNeeds
        result `shouldBe` [CreateIndex "album" ["format"]]

      it "skips empty column lists from query needs" do
        let queryNeeds = [("album", [])]
        optimizeIndexes [] queryNeeds `shouldBe` []

    -- Combined scenarios
    describe "combined optimization" do
      it "handles redundant drop + missing create together" do
        let idx1 = mkIndex "idx1" "album" ["name"] False False
            idx2 = mkIndex "idx2" "album" ["name", "format"] False False
            queryNeeds = [("album", ["released"])]
            result = optimizeIndexes [idx1, idx2] queryNeeds
            drops = [a | a@(DropIndex _ _) <- result]
            creates = [a | a@(CreateIndex {}) <- result]
        length drops `shouldSatisfy` (>= 1)
        length creates `shouldBe` 1
        case creates of
          [CreateIndex tbl cols] -> do
            tbl `shouldBe` "album"
            cols `shouldBe` ["released"]
          _ -> expectationFailure "Expected single CreateIndex"

      it "accounts for excessive-composite replacement when checking missing" do
        -- If we have idx on (name, format) and only query "name",
        -- the excessive detection replaces it with (name).
        -- A query needing "name" should NOT generate a missing-index create
        -- because the replacement covers it.
        let idx = mkIndex "idx_nf" "album" ["name", "format"] False False
            queryNeeds = [("album", ["name"])]
            result = optimizeIndexes [idx] queryNeeds
            creates = [a | a@(CreateIndex {}) <- result]
        creates `shouldBe` []

      it "handles multiple tables independently" do
        let idx1 = mkIndex "idx_a1" "users" ["email"] False False
            idx2 = mkIndex "idx_a2" "users" ["email"] False False
            idx3 = mkIndex "idx_b1" "orders" ["status", "date"] False False
            queryNeeds = [("orders", ["status"]), ("users", ["name"])]
            result = optimizeIndexes [idx1, idx2, idx3] queryNeeds
            drops = [a | a@(DropIndex _ _) <- result]
            creates = [a | a@(CreateIndex {}) <- result]
        -- idx_a2 is duplicate of idx_a1
        -- idx_b1 is excessive (only "status" needed)
        -- "users" needs index on "name"
        length drops `shouldSatisfy` (>= 2)
        creates `shouldSatisfy` any (\case CreateIndex t c -> t == "users" && c == ["name"]; _ -> False)

    -- Edge cases for composite index narrowing with multiple queries
    describe "composite narrowing edge cases" do
      it "only considers contiguous leading prefix for narrowing" do
        -- Index on (a, b, c), query needs [a, c] (gap at b).
        -- Only 1 leading column is usable, so narrow to [a].
        let idx = mkIndex "idx_abc" "t" ["a", "b", "c"] False False
            queryNeeds = [("t", ["a", "c"])]
            result = optimizeIndexes [idx] queryNeeds
            drops = [a | a@(DropIndex _ (ExcessiveComposite _)) <- result]
        case drops of
          [DropIndex _ (ExcessiveComposite replacement)] ->
            replacement `shouldBe` ["a"]
          _ -> expectationFailure "Expected ExcessiveComposite with [a]"

      it "does not narrow if query uses no leading columns" do
        -- Index on (a, b), query needs [b] alone — btree can't help with just b.
        -- No narrowing should happen (prefix needed = 0).
        let idx = mkIndex "idx_ab" "t" ["a", "b"] False False
            queryNeeds = [("t", ["b"])]
            result = optimizeIndexes [idx] queryNeeds
            excessiveDrops = [a | a@(DropIndex _ (ExcessiveComposite _)) <- result]
        excessiveDrops `shouldBe` []

      it "handles disjoint query sets requiring separate indexes" do
        -- Query A needs [email], Query B needs [name] on same table.
        -- An existing index on [email, name] covers A via prefix(1),
        -- but does NOT cover B (name is not a prefix).
        let idx = mkIndex "idx_en" "users" ["email", "name"] False False
            queryNeeds = [("users", ["email"]), ("users", ["name"])]
            result = optimizeIndexes [idx] queryNeeds
            creates = [a | a@(CreateIndex {}) <- result]
        -- Should suggest a missing index for "name"
        creates `shouldSatisfy` any (\case CreateIndex _ c -> c == ["name"]; _ -> False)

      it "does not narrow when all columns are needed across queries" do
        -- Three queries each using a deeper prefix.
        let idx = mkIndex "idx_abc" "t" ["a", "b", "c"] False False
            queryNeeds = [("t", ["a"]), ("t", ["a", "b"]), ("t", ["a", "b", "c"])]
        optimizeIndexes [idx] queryNeeds `shouldBe` []

      it "narrows to 2 when one query needs [a] and another needs [a,b] out of [a,b,c]" do
        let idx = mkIndex "idx_abc" "t" ["a", "b", "c"] False False
            queryNeeds = [("t", ["a"]), ("t", ["a", "b"])]
            result = optimizeIndexes [idx] queryNeeds
            drops = [a | a@(DropIndex _ (ExcessiveComposite _)) <- result]
        case drops of
          [DropIndex _ (ExcessiveComposite replacement)] ->
            replacement `shouldBe` ["a", "b"]
          _ -> expectationFailure "Expected ExcessiveComposite with [a, b]"

  describe "generateMigration" do
    it "generates DROP and CREATE statements in a single migration" do
      let actions =
            [ DropIndex (mkIndex "idx_dup" "users" ["email"] False False) (ExactDuplicate (mkIndex "idx_orig" "users" ["email"] False False)),
              CreateIndex "album" ["format"]
            ]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "DROP INDEX" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "CREATE INDEX" `Text.isInfixOf` t

    it "includes replacement indexes for excessive composites" do
      let idx = mkIndex "idx_nf" "album" ["name", "format"] False False
          actions = [DropIndex idx (ExcessiveComposite ["name"])]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "DROP INDEX" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "CREATE INDEX ON album (name)" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "replacing with" `Text.isInfixOf` t

    it "generates empty migration for no actions" do
      generateMigration [] `shouldSatisfy` \t -> not ("DROP" `Text.isInfixOf` t || "CREATE" `Text.isInfixOf` t)

    it "handles prefix-redundancy drop with comment" do
      let idx = mkIndex "idx_a" "users" ["email"] False False
          superseder = mkIndex "idx_ab" "users" ["email", "name"] False False
          actions = [DropIndex idx (PrefixRedundancy superseder)]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "DROP INDEX" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "is a prefix of" `Text.isInfixOf` t

    it "handles unused-by-queries drop with comment" do
      let idx = mkIndex "idx_stale" "album" ["format"] False False
          actions = [DropIndex idx UnusedByQueries]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "DROP INDEX" `Text.isInfixOf` t
      migration `shouldSatisfy` \t -> "not used by observed query needs" `Text.isInfixOf` t

    it "quotes schema and index names in DROP statements" do
      let idx = mkIndex "my-index" "album" ["name"] False False
          actions = [DropIndex idx (ExactDuplicate idx)]
          migration = generateMigration actions
      migration `shouldSatisfy` \t -> "\"public\".\"my-index\"" `Text.isInfixOf` t

    it "generates all sections in correct order" do
      let redIdx = mkIndex "idx_red" "users" ["email"] False False
          compIdx = mkIndex "idx_comp" "users" ["name", "age"] False False
          actions =
            [ DropIndex redIdx (ExactDuplicate (mkIndex "idx_orig" "users" ["email"] False False)),
              DropIndex compIdx (ExcessiveComposite ["name"]),
              CreateIndex "orders" ["status"]
            ]
          migration = generateMigration actions
          -- Find positions of each section
          findPos needle haystack = Text.length (fst (Text.breakOn needle haystack))
          dropPos = findPos "Drop redundant" migration
          replPos = findPos "replacement" migration
          createPos = findPos "missing" migration
      -- Drops section comes before replacement creates which come before missing creates
      dropPos `shouldSatisfy` (< replPos)
      replPos `shouldSatisfy` (< createPos)

-- * Helpers

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
