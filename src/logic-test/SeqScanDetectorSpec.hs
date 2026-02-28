module SeqScanDetectorSpec (spec) where

import Base.Prelude
import Data.Text qualified as Text
import Logic.Algebra (SeqScanFinding (..))
import Logic.SeqScanDetector
import Test.Hspec

spec :: Spec
spec = do
  describe "detectSeqScans" do
    it "detects a seq scan with filter" do
      let explainOutput =
            [ "Seq Scan on album  (cost=0.00..1.04 rows=1 width=100)",
              "  Filter: (format = $1)"
            ]
      detectSeqScans explainOutput
        `shouldBe` [SeqScanFinding "album" "(format = $1)" ["format"]]

    it "ignores seq scan without filter" do
      let explainOutput =
            [ "Seq Scan on album  (cost=0.00..1.04 rows=5 width=100)"
            ]
      detectSeqScans explainOutput `shouldBe` []

    it "detects multiple seq scans in nested plan" do
      let explainOutput =
            [ "Nested Loop  (cost=0.00..100.00 rows=10 width=200)",
              "  ->  Seq Scan on album  (cost=0.00..1.04 rows=1 width=100)",
              "        Filter: (format = $1)",
              "  ->  Seq Scan on artist  (cost=0.00..1.04 rows=1 width=100)",
              "        Filter: (name = $2)"
            ]
      length (detectSeqScans explainOutput) `shouldBe` 2

    it "extracts table name correctly" do
      let explainOutput =
            [ "  Seq Scan on album_genre  (cost=0.00..1.04 rows=1 width=50)",
              "    Filter: (album = $1)"
            ]
      case detectSeqScans explainOutput of
        [finding] -> finding.tableName `shouldBe` "album_genre"
        findings -> expectationFailure $ "Expected 1 finding, got " <> show (length findings)

    it "handles indented seq scan from subquery" do
      let explainOutput =
            [ "Hash Join  (cost=0.00..100.00 rows=10 width=200)",
              "  Hash Cond: (a.id = b.id)",
              "  ->  Seq Scan on users  (cost=0.00..1.04 rows=1 width=100)",
              "        Filter: (active = $1)",
              "  ->  Hash",
              "        ->  Seq Scan on orders  (cost=0.00..1.04 rows=1 width=100)"
            ]
      case detectSeqScans explainOutput of
        [finding] -> do
          finding.tableName `shouldBe` "users"
          finding.filterCondition `shouldBe` "(active = $1)"
        findings -> expectationFailure $ "Expected 1 finding, got " <> show (length findings)

  describe "extractFilterColumns" do
    it "extracts single column from simple equality" do
      extractFilterColumns "(format = $1)" `shouldBe` ["format"]

    it "extracts columns from AND condition" do
      let cols = extractFilterColumns "((format = $1) AND (released = $2))"
      cols `shouldContain` ["format"]
      cols `shouldContain` ["released"]

    it "extracts column from qualified name" do
      extractFilterColumns "(album_artist.artist = $1)" `shouldBe` ["artist"]

    it "returns empty for no identifiable columns" do
      extractFilterColumns "($1 = $2)" `shouldBe` []

  describe "generateCreateIndexStatements" do
    it "generates CREATE INDEX for findings with columns" do
      let findings =
            [ SeqScanFinding "album" "(format = $1)" ["format"]
            ]
      generateCreateIndexStatements findings
        `shouldSatisfy` Text.isInfixOf "CREATE INDEX ON album (format);"

    it "generates TODO comment for findings without columns" do
      let findings =
            [ SeqScanFinding "album" "($1 = $2)" []
            ]
      generateCreateIndexStatements findings
        `shouldSatisfy` Text.isInfixOf "TODO"

    it "generates multiple indexes" do
      let findings =
            [ SeqScanFinding "album" "(format = $1)" ["format"],
              SeqScanFinding "artist" "(name = $1)" ["name"]
            ]
          result = generateCreateIndexStatements findings
      result `shouldSatisfy` Text.isInfixOf "CREATE INDEX ON album (format);"
      result `shouldSatisfy` Text.isInfixOf "CREATE INDEX ON artist (name);"
