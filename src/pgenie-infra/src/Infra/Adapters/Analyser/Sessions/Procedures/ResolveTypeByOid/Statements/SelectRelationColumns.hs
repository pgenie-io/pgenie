{-# LANGUAGE QuasiQuotes #-}

-- | Hasql statement fetching a relation's columns by relation name.
module Infra.Adapters.Analyser.Sessions.Procedures.ResolveTypeByOid.Statements.SelectRelationColumns where

import Hasql.Mapping.IsStatement
import Hasql.TH
import Utils.Prelude

-- | The relation name whose columns to fetch.
data SelectRelationColumnsParams = SelectRelationColumnsParams
  { -- | Relation name.
    name :: Text
  }
  deriving stock (Eq, Show)

-- | The relation's columns, in attribute order.
type SelectRelationColumnsResult = Vector SelectRelationColumnsResultRow

-- | One column of a relation.
data SelectRelationColumnsResultRow = SelectRelationColumnsResultRow
  { name :: Text,
    typeId :: Int32,
    -- | Number of dimensions, if the column is an array type; otherwise 0. (Presently, the number of dimensions of an array is not enforced, so any nonzero value effectively means “it's an array”.)
    nDims :: Int32,
    notNull :: Bool
  }

instance IsStatement SelectRelationColumnsParams where
  type Result SelectRelationColumnsParams = SelectRelationColumnsResult
  statement =
    [vectorStatement|
      select attname :: text, atttypid :: int4, attndims :: int4, attnotnull :: bool
      from pg_class
      left join pg_attribute on attrelid = pg_class.oid
      where pg_class.relname = $1 :: text
      order by attnum
    |]
      & lmap (.name)
      & (rmap . fmap) (uncurryN SelectRelationColumnsResultRow)
