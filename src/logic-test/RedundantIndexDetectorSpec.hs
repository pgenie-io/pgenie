module RedundantIndexDetectorSpec (spec) where

import Base.Prelude
import Logic.Algebra
import Logic.RedundantIndexDetector
import Test.Hspec

spec :: Spec
spec = do
  describe "detectRedundantIndexes" do
    it "returns empty list when no indexes are provided" do
      detectRedundantIndexes [] `shouldBe` []

    it "returns empty list for a single index" do
      let indexes = [mkIndex "idx1" "users" ["id"] False False]
      detectRedundantIndexes indexes `shouldBe` []

    it "detects exact duplicate indexes" do
      let idx1 = mkIndex "idx1" "users" ["email"] False False
          idx2 = mkIndex "idx2" "users" ["email"] False False
          result = detectRedundantIndexes [idx1, idx2]
      length result `shouldBe` 1
      (.index.indexName) (head result) `shouldBe` "idx2"
      case (.reason) (head result) of
        ExactDuplicate other -> other.indexName `shouldBe` "idx1"
        _ -> expectationFailure "Expected ExactDuplicate"

    it "detects prefix redundancy" do
      let idx1 = mkIndex "idx_a" "users" ["email"] False False
          idx2 = mkIndex "idx_ab" "users" ["email", "name"] False False
          result = detectRedundantIndexes [idx1, idx2]
      length result `shouldBe` 1
      (.index.indexName) (head result) `shouldBe` "idx_a"
      case (.reason) (head result) of
        PrefixRedundancy other -> other.indexName `shouldBe` "idx_ab"
        _ -> expectationFailure "Expected PrefixRedundancy"

    it "does not flag indexes on different tables" do
      let idx1 = mkIndex "idx1" "users" ["email"] False False
          idx2 = mkIndex "idx2" "orders" ["email"] False False
      detectRedundantIndexes [idx1, idx2] `shouldBe` []

    it "does not flag indexes with different methods" do
      let idx1 = mkIndexWithMethod "idx1" "users" ["data"] "btree" False False
          idx2 = mkIndexWithMethod "idx2" "users" ["data"] "gin" False False
      detectRedundantIndexes [idx1, idx2] `shouldBe` []

    it "does not flag primary key indexes as redundant" do
      let idx1 = mkIndex "pk_users" "users" ["id"] False True
          idx2 = mkIndex "idx_users_id" "users" ["id"] False False
      -- Only the non-primary should be flagged (as duplicate of the primary)
      let result = detectRedundantIndexes [idx1, idx2]
      length result `shouldBe` 1
      (.index.indexName) (head result) `shouldBe` "idx_users_id"

    it "does not flag indexes with different predicates" do
      let idx1 = mkIndexWithPredicate "idx1" "users" ["email"] Nothing
          idx2 = mkIndexWithPredicate "idx2" "users" ["email"] (Just "(active = true)")
      detectRedundantIndexes [idx1, idx2] `shouldBe` []

    it "detects prefix redundancy with matching predicates" do
      let idx1 = mkIndexWithPredicate "idx1" "users" ["email"] (Just "(active = true)")
          idx2 = mkIndexWithPredicate "idx2" "users" ["email", "name"] (Just "(active = true)")
          result = detectRedundantIndexes [idx1, idx2]
      length result `shouldBe` 1
      (.index.indexName) (head result) `shouldBe` "idx1"

    it "does not flag non-prefix column overlap" do
      let idx1 = mkIndex "idx1" "users" ["name"] False False
          idx2 = mkIndex "idx2" "users" ["email", "name"] False False
      detectRedundantIndexes [idx1, idx2] `shouldBe` []

    it "handles multiple redundancies" do
      let idxA = mkIndex "idx_a" "users" ["email"] False False
          idxAB = mkIndex "idx_ab" "users" ["email", "name"] False False
          idxABC = mkIndex "idx_abc" "users" ["email", "name", "age"] False False
          result = detectRedundantIndexes [idxA, idxAB, idxABC]
      -- idx_a is prefix of idx_ab (or idx_abc), idx_ab is prefix of idx_abc
      length result `shouldBe` 2

  describe "generateDropMigration" do
    it "generates valid SQL for dropping redundant indexes" do
      let idx1 = mkIndex "idx_redundant" "users" ["email"] False False
          idx2 = mkIndex "idx_full" "users" ["email", "name"] False False
          redundant =
            [ RedundantIndex
                { index = idx1,
                  reason = PrefixRedundancy idx2
                }
            ]
          migration = generateDropMigration redundant
      migration `shouldSatisfy` \t ->
        "DROP INDEX \"public\".\"idx_redundant\";" `isInfixOf` to t

    it "generates comments explaining the reason" do
      let idx1 = mkIndex "idx_dup" "users" ["email"] False False
          idx2 = mkIndex "idx_orig" "users" ["email"] False False
          redundant =
            [ RedundantIndex
                { index = idx1,
                  reason = ExactDuplicate idx2
                }
            ]
          migration = generateDropMigration redundant
      migration `shouldSatisfy` \t ->
        "exact duplicate" `isInfixOf` to t

-- | Helper to create an IndexInfo with defaults.
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
mkIndexWithPredicate name table cols pred =
  (mkIndex name table cols False False) {predicate = pred}
