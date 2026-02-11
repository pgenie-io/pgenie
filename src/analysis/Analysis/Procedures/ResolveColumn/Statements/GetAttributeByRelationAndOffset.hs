module Analysis.Procedures.ResolveColumn.Statements.GetAttributeByRelationAndOffset where

import Base.Prelude
import Hasql.Mapping.IsStatement
import Hasql.TH

data GetAttributeByRelationAndOffsetParams = GetAttributeByRelationAndOffsetParams
  { relationOid :: Int32,
    attributeNum :: Int32
  }
  deriving stock (Eq, Show)

type GetAttributeByRelationAndOffsetResult = Maybe GetAttributeByRelationAndOffsetResultRow

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
