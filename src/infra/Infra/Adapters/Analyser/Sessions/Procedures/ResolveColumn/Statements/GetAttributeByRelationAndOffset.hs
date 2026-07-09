{-# LANGUAGE QuasiQuotes #-}

-- | Hasql statement fetching one @pg_attribute@ row by relation OID and attribute offset.
module Infra.Adapters.Analyser.Sessions.Procedures.ResolveColumn.Statements.GetAttributeByRelationAndOffset where

import Hasql.Mapping.IsStatement
import Hasql.TH
import Utils.Prelude

-- | The relation and attribute offset to look up.
data GetAttributeByRelationAndOffsetParams = GetAttributeByRelationAndOffsetParams
  { relationOid :: Int32,
    attributeNum :: Int32
  }
  deriving stock (Eq, Show)

-- | Present when the relation and offset refer to an existing attribute.
type GetAttributeByRelationAndOffsetResult = Maybe GetAttributeByRelationAndOffsetResultRow

-- | The matched attribute's name, type, dimensionality, and nullability.
data GetAttributeByRelationAndOffsetResultRow = GetAttributeByRelationAndOffsetResultRow
  { -- | The name of the attribute.
    name :: Text,
    -- | OID of the type of the attribute.
    typeId :: Int32,
    -- | Number of dimensions of the type.
    nDims :: Int32,
    -- | Whether it's not nullable.
    notNull :: Bool
  }

instance IsStatement GetAttributeByRelationAndOffsetParams where
  type Result GetAttributeByRelationAndOffsetParams = GetAttributeByRelationAndOffsetResult
  statement =
    [maybeStatement|
      select
        attname :: text,
        atttypid :: int4,
        attndims :: int4,
        attnotnull :: bool
      from pg_attribute
      where
        attrelid = $1 :: int4 and
        attnum = $2 :: int4
      limit 1
    |]
      & lmap (\params -> (params.relationOid, params.attributeNum))
      & (rmap . fmap) (uncurryN GetAttributeByRelationAndOffsetResultRow)
