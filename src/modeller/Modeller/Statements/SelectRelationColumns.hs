module Modeller.Statements.SelectRelationColumns where

import Base.Prelude
import Hasql.TH
import HasqlDev

data SelectRelationColumnsParams = SelectRelationColumnsParams
  { -- | Relation name.
    name :: Text
  }
  deriving stock (Eq, Show)

type SelectRelationColumnsResult = Vector SelectRelationColumnsResultRow

data SelectRelationColumnsResultRow = SelectRelationColumnsResultRow
  { name :: Text,
    typeId :: Int32,
    -- | Number of dimensions, if the column is an array type; otherwise 0. (Presently, the number of dimensions of an array is not enforced, so any nonzero value effectively means “it's an array”.)
    nDims :: Int32,
    notNull :: Bool
  }

instance IsStatementParams SelectRelationColumnsParams where
  type StatementResultByParams SelectRelationColumnsParams = SelectRelationColumnsResult
  statementByParams =
    [vectorStatement|
      select attname :: text, atttypid :: int4, attndims :: int4, attnotnull :: bool
      from pg_class
      left join pg_attribute on attrelid = pg_class.oid
      where pg_class.relname = $1 :: text
      order by attnum
    |]
      & lmap (.name)
      & (rmap . fmap) (uncurryN SelectRelationColumnsResultRow)
