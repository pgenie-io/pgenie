-- |
-- Fetches the catalog of user-defined indexes, used to evaluate whether a
-- query benefits from an index that isn't already in place.
module Infra.Adapters.Analyser.Sessions.Procedures.GetIndexes
  ( getIndexes,
  )
where

import Hasql.Decoders qualified
import Hasql.Session qualified
import Hasql.Statement qualified
import Logic.Domain.IndexOptimization (IndexInfo (..))
import Utils.Prelude

-- | Fetch all user-defined indexes from the database.
getIndexes :: Hasql.Session.Session [IndexInfo]
getIndexes =
  Hasql.Session.statement () indexesStatement
  where
    indexesStatement :: Hasql.Statement.Statement () [IndexInfo]
    indexesStatement =
      Hasql.Statement.unpreparable sql mempty decoder
      where
        sql =
          mconcat
            [ "SELECT ",
              "  ci.relname::text, ",
              "  ct.relname::text, ",
              "  n.nspname::text, ",
              "  array_agg(a.attname::text ORDER BY k.ord), ",
              "  ix.indisunique, ",
              "  ix.indisprimary, ",
              "  am.amname::text, ",
              "  pg_get_expr(ix.indpred, ix.indrelid) ",
              "FROM pg_index ix ",
              "JOIN pg_class ci ON ci.oid = ix.indexrelid ",
              "JOIN pg_class ct ON ct.oid = ix.indrelid ",
              "JOIN pg_namespace n ON n.oid = ct.relnamespace ",
              "JOIN pg_am am ON am.oid = ci.relam ",
              "CROSS JOIN LATERAL unnest(ix.indkey::smallint[]) WITH ORDINALITY AS k(attnum, ord) ",
              "JOIN pg_attribute a ON a.attrelid = ct.oid AND a.attnum = k.attnum ",
              "WHERE n.nspname = 'public' ",
              "AND ct.relkind = 'r' ",
              "AND k.attnum > 0 ",
              "GROUP BY ci.relname, ct.relname, n.nspname, ix.indisunique, ix.indisprimary, am.amname, ix.indrelid, ix.indpred ",
              "ORDER BY ct.relname, ci.relname"
            ]

        decoder :: Hasql.Decoders.Result [IndexInfo]
        decoder =
          Hasql.Decoders.rowList rowDecoder

        rowDecoder :: Hasql.Decoders.Row IndexInfo
        rowDecoder = do
          indexName <- Hasql.Decoders.column (Hasql.Decoders.nonNullable Hasql.Decoders.text)
          tableName <- Hasql.Decoders.column (Hasql.Decoders.nonNullable Hasql.Decoders.text)
          schemaName <- Hasql.Decoders.column (Hasql.Decoders.nonNullable Hasql.Decoders.text)
          columnsArray <- Hasql.Decoders.column (Hasql.Decoders.nonNullable (Hasql.Decoders.listArray (Hasql.Decoders.nonNullable Hasql.Decoders.text)))
          isUnique <- Hasql.Decoders.column (Hasql.Decoders.nonNullable Hasql.Decoders.bool)
          isPrimary <- Hasql.Decoders.column (Hasql.Decoders.nonNullable Hasql.Decoders.bool)
          indexMethod <- Hasql.Decoders.column (Hasql.Decoders.nonNullable Hasql.Decoders.text)
          predicate <- Hasql.Decoders.column (Hasql.Decoders.nullable Hasql.Decoders.text)
          pure
            IndexInfo
              { indexName,
                tableName,
                schemaName,
                columns = columnsArray,
                isUnique,
                isPrimary,
                indexMethod,
                predicate
              }
