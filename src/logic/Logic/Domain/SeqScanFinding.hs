module Logic.Domain.SeqScanFinding
  ( SeqScanFinding (..),
    detectSeqScans,
    extractFilterColumns,
    spec,
  )
where

import Data.Text qualified as Text
import Test.Hspec
import Utils.Prelude

-- | A finding of sequential scan in a query execution plan.
data SeqScanFinding = SeqScanFinding
  { -- | Name of the table being sequentially scanned.
    tableName :: Text,
    -- | The filter condition from the EXPLAIN output.
    filterCondition :: Text,
    -- | Suggested columns to create an index on.
    suggestedIndexColumns :: [Text]
  }
  deriving stock (Eq, Show)

-- | Detect sequential scans with filters from EXPLAIN text output lines.
-- Only seq scans that have a filter condition are flagged,
-- because those indicate a missing index. Full table scans without
-- a filter are expected for queries that need all rows.
detectSeqScans :: [Text] -> [SeqScanFinding]
detectSeqScans = go
  where
    go [] = []
    go [_] = []
    go (line : nextLine : rest)
      | Just (indent, tableName) <- parseSeqScanLine line,
        Just condition <- parseFilterLine indent nextLine =
          let columns = extractFilterColumns condition
           in SeqScanFinding {tableName, filterCondition = condition, suggestedIndexColumns = columns}
                : go rest
      | otherwise = go (nextLine : rest)

-- | Parse a line for "Seq Scan on <table>" pattern.
-- Returns the indentation level and table name.
parseSeqScanLine :: Text -> Maybe (Int, Text)
parseSeqScanLine line =
  let indent = Text.length (Text.takeWhile (== ' ') line)
      trimmed = Text.strip line
      -- Remove plan tree arrow prefix if present (e.g., "->  ")
      withoutArrow = case Text.stripPrefix "-> " trimmed of
        Just rest -> Text.stripStart rest
        Nothing -> trimmed
   in case Text.stripPrefix "Seq Scan on " withoutArrow of
        Just rest ->
          let tableName = Text.takeWhile (\c -> c /= ' ' && c /= '(') rest
           in if Text.null tableName then Nothing else Just (indent, tableName)
        Nothing -> Nothing

-- | Parse a Filter line that is a child of a parent node.
-- The line must be indented more than the parent.
parseFilterLine :: Int -> Text -> Maybe Text
parseFilterLine parentIndent line =
  let indent = Text.length (Text.takeWhile (== ' ') line)
      trimmed = Text.strip line
   in if indent > parentIndent
        then Text.stripPrefix "Filter: " trimmed
        else Nothing

-- | Extract column names from a filter condition.
-- Handles common patterns like:
-- - @(format = $1)@
-- - @((id = $1) AND (name = $2))@
-- - @(album_artist.artist = $1)@
extractFilterColumns :: Text -> [Text]
extractFilterColumns condition =
  let subExprs = splitOnKeywords condition
   in nub (mapMaybe extractFirstColumn subExprs)
  where
    splitOnKeywords text =
      text
        & Text.replace " AND " "\0"
        & Text.replace " OR " "\0"
        & Text.splitOn "\0"

    extractFirstColumn expr =
      let cleaned = Text.dropWhile (\c -> c == '(' || c == ' ') expr
          ident = Text.takeWhile (\c -> isAlphaNum c || c == '_' || c == '.') cleaned
       in if Text.null ident
            || Text.isPrefixOf "$" ident
            || Text.toUpper ident
            `elem` ["NOT", "IS", "NULL", "TRUE", "FALSE", "ANY", "ALL"]
            then Nothing
            else Just (extractColumnPart ident)

    -- For qualified names "table.column", take just "column".
    extractColumnPart ident =
      case Text.splitOn "." ident of
        [_table, col] -> col
        _ -> ident

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

    it "does not flag a CTE Scan as a sequential scan" do
      let explainOutput =
            [ "CTE Scan on inserted  (cost=0.00..0.02 rows=1 width=100)",
              "  Filter: (status = $1)"
            ]
      detectSeqScans explainOutput `shouldBe` []

    it "does not flag a Function Scan as a sequential scan" do
      let explainOutput =
            [ "Function Scan on unnest v  (cost=0.00..1.00 rows=100 width=32)",
              "  Filter: (v = $1)"
            ]
      detectSeqScans explainOutput `shouldBe` []

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
