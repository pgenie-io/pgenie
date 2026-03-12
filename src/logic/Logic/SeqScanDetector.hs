module Logic.SeqScanDetector
  ( SeqScanFinding (..),
    detectSeqScans,
    extractFilterColumns,
    inferSeqScanFindingsFromSql,
  )
where

import Base.Prelude
import Data.Set qualified as Set
import Data.Text qualified as Text

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

-- | Infer seq-scan findings from a SQL query text by extracting table and
-- WHERE clause information.  This is a fallback when EXPLAIN is not available.
inferSeqScanFindingsFromSql :: Text -> [SeqScanFinding]
inferSeqScanFindingsFromSql sql =
  case inferTableAndWhere sql of
    Nothing -> []
    Just (tbl, whereClause) ->
      let cols = extractFilterColumns whereClause
       in if null cols
            then []
            else [SeqScanFinding tbl whereClause cols]

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

inferTableName :: [Text] -> Maybe Text
inferTableName tokens =
  let lowerTokens = map Text.toLower tokens
      findAfterKeyword kw = do
        i <- elemIndex kw lowerTokens
        token <- tokens !? (i + 1)
        pure (normalizeTableToken token)
   in findAfterKeyword "from"
        <|> findAfterKeyword "update"
        <|> ( do
                i <- elemIndex "into" lowerTokens
                prev <- lowerTokens !? (i - 1)
                guard (prev == "insert")
                token <- tokens !? (i + 1)
                pure (normalizeTableToken token)
            )

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
