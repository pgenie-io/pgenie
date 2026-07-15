{-# LANGUAGE QuasiQuotes #-}

-- | Hasql statement fetching an enum type's ordered labels by its OID.
module Infra.Adapters.Analyser.Sessions.Procedures.ResolveTypeByOid.Statements.SelectEnumLabels where

import Hasql.Mapping.IsStatement
import Hasql.TH
import Utils.Prelude

-- | The enum type OID whose labels to fetch.
data SelectEnumLabelsParams = SelectEnumLabelsParams
  { -- | The object identifier of the type.
    oid :: Int32
  }
  deriving stock (Eq, Show)

-- | The enum's labels, in sort order.
type SelectEnumLabelsResult = Vector Text

instance IsStatement SelectEnumLabelsParams where
  type Result SelectEnumLabelsParams = SelectEnumLabelsResult
  statement =
    [vectorStatement|
      select enumlabel :: text
      from pg_enum
      where enumtypid = $1 :: int4
      order by enumsortorder
    |]
      & lmap (.oid)
