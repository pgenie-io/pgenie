module Logic.RedundantIndexDetector
  ( detectRedundantIndexes,
    generateDropMigration,
  )
where

import Base.Prelude
import Data.Text qualified as Text
import Logic.Algebra

-- | Detect redundant indexes from a list of index definitions.
--
-- An index is considered redundant if:
--
-- 1. Its columns are an exact duplicate of another index on the same table
--    with the same method and predicate.
-- 2. Its columns are a leading prefix of another index on the same table
--    with the same method and predicate.
--
-- Primary key indexes are never considered redundant.
-- For exact duplicates, only the later index (by name) is flagged.
detectRedundantIndexes :: [IndexInfo] -> [RedundantIndex]
detectRedundantIndexes indexes =
  let nonPrimary = filter (not . (.isPrimary)) indexes
   in concatMap (findRedundancies indexes) nonPrimary

-- | Check if a given index is redundant with respect to any other index.
findRedundancies :: [IndexInfo] -> IndexInfo -> [RedundantIndex]
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
        Just (reason, _superseder) -> [RedundantIndex candidate reason]

-- | Find the first index that supersedes the candidate.
-- Returns the reason and the superseding index.
-- For exact duplicates between two non-primary indexes, the candidate is only
-- flagged if its name is lexicographically greater, so only one side of a
-- duplicate pair is reported. A primary key always supersedes a non-primary duplicate.
findSuperseding :: IndexInfo -> [IndexInfo] -> Maybe (RedundancyReason, IndexInfo)
findSuperseding candidate = go
  where
    go [] = Nothing
    go (other : rest)
      | candidate.columns == other.columns
          && (other.isPrimary || candidate.indexName > other.indexName) =
          Just (ExactDuplicate other, other)
      | isPrefix candidate.columns other.columns =
          Just (PrefixRedundancy other, other)
      | otherwise = go rest

-- | Check if the first list is a strict prefix of the second.
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] (_ : _) = True
isPrefix _ [] = False
isPrefix (x : xs) (y : ys) = x == y && isPrefix xs ys

-- | Generate a SQL migration that drops the given redundant indexes.
generateDropMigration :: [RedundantIndex] -> Text
generateDropMigration redundantIndexes =
  Text.unlines
    ( ["-- Auto-generated migration to drop redundant indexes", ""]
        <> concatMap dropStatement redundantIndexes
    )
  where
    dropStatement :: RedundantIndex -> [Text]
    dropStatement ri =
      [ "-- " <> reasonComment ri,
        "DROP INDEX " <> quoteIdent ri.index.schemaName <> "." <> quoteIdent ri.index.indexName <> ";",
        ""
      ]

    reasonComment :: RedundantIndex -> Text
    reasonComment ri = case ri.reason of
      ExactDuplicate other ->
        ri.index.indexName <> " is an exact duplicate of " <> other.indexName
      PrefixRedundancy other ->
        ri.index.indexName <> " is a prefix of " <> other.indexName

    quoteIdent :: Text -> Text
    quoteIdent t = "\"" <> Text.replace "\"" "\"\"" t <> "\""
