module Infra.Adapters.Analyser.Sessions.Procedures.GetIndexes where

import Hasql.Decoders qualified as Decoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Logic qualified
import Utils.Prelude

-- | Fetch all user-defined indexes from the database.
getIndexes :: Session.Session [Logic.IndexInfo]
getIndexes =
  Session.statement () indexesStatement
  where
    indexesStatement :: Statement.Statement () [Logic.IndexInfo]
    indexesStatement =
      Statement.unpreparable sql mempty decoder
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

        decoder :: Decoders.Result [Logic.IndexInfo]
        decoder =
          Decoders.rowList rowDecoder

        rowDecoder :: Decoders.Row Logic.IndexInfo
        rowDecoder = do
          indexName <- Decoders.column (Decoders.nonNullable Decoders.text)
          tableName <- Decoders.column (Decoders.nonNullable Decoders.text)
          schemaName <- Decoders.column (Decoders.nonNullable Decoders.text)
          columnsArray <- Decoders.column (Decoders.nonNullable (Decoders.listArray (Decoders.nonNullable Decoders.text)))
          isUnique <- Decoders.column (Decoders.nonNullable Decoders.bool)
          isPrimary <- Decoders.column (Decoders.nonNullable Decoders.bool)
          indexMethod <- Decoders.column (Decoders.nonNullable Decoders.text)
          predicate <- Decoders.column (Decoders.nullable Decoders.text)
          pure
            Logic.IndexInfo
              { indexName,
                tableName,
                schemaName,
                columns = columnsArray,
                isUnique,
                isPrimary,
                indexMethod,
                predicate
              }
