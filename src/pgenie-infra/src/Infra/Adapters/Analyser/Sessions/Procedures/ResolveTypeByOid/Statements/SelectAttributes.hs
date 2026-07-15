{-# LANGUAGE QuasiQuotes #-}

-- | Hasql statement fetching the attributes of a composite type by its OID.
module Infra.Adapters.Analyser.Sessions.Procedures.ResolveTypeByOid.Statements.SelectAttributes where

import Hasql.Mapping.IsStatement
import Hasql.TH
import Utils.Prelude

-- | The composite type OID whose attributes to fetch.
data SelectAttributesParams = SelectAttributesParams
  { -- | The object identifier of the type.
    oid :: Int32
  }
  deriving stock (Eq, Show)

-- | The composite type's fields, in declaration order.
type SelectAttributesResult = Vector SelectAttributesResultRow

-- | One field of a composite type.
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

instance IsStatement SelectAttributesParams where
  type Result SelectAttributesParams = SelectAttributesResult
  statement =
    [vectorStatement|
      select attname :: text, atttypid :: int4, attndims :: int4, attnotnull :: bool
      from pg_type
      join pg_attribute on pg_attribute.attrelid = pg_type.typrelid
      where pg_type.oid = $1 :: int4
        and pg_attribute.attnum > 0
      order by attnum
    |]
      & lmap (.oid)
      & (rmap . fmap) (uncurryN SelectAttributesResultRow)
