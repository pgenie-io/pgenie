module Modeller.Procedures.ResolveTypeByOid.Statements.SelectAttributes where

import Base.Prelude
import Hasql.TH
import HasqlDev

data SelectAttributesParams = SelectAttributesParams
  { -- | The object identifier of the type.
    oid :: Int32
  }
  deriving stock (Eq, Show)

type SelectAttributesResult = Vector SelectAttributesResultRow

data SelectAttributesResultRow = SelectAttributesResultRow
  { -- | The name of the attribute.
    name :: Text,
    -- | OID of the type of the attribute.
    typeId :: Int32,
    -- | Number of dimensions of the type.
    nDims :: Int32,
    -- | Whether it's not nullable.
    notNull :: Bool
  }

instance IsStatementParams SelectAttributesParams where
  type StatementResultByParams SelectAttributesParams = SelectAttributesResult
  statementByParams =
    [vectorStatement|
      select attname :: text, atttypid :: int4, attndims :: int4, attnotnull :: bool
      from pg_type
      join pg_attribute on pg_attribute.attrelid = pg_type.typrelid
      where pg_type.oid = $1 :: int4
      order by attnum
    |]
      & lmap (.oid)
      & (rmap . fmap) (uncurryN SelectAttributesResultRow)
