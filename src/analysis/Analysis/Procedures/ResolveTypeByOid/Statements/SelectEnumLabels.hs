module Analysis.Procedures.ResolveTypeByOid.Statements.SelectEnumLabels where

import Base.Prelude
import Hasql.TH
import HasqlDev

data SelectEnumLabelsParams = SelectEnumLabelsParams
  { -- | The object identifier of the type.
    oid :: Int32
  }
  deriving stock (Eq, Show)

type SelectEnumLabelsResult = Vector Text

instance IsStatementParams SelectEnumLabelsParams where
  type StatementResultByParams SelectEnumLabelsParams = SelectEnumLabelsResult
  statementByParams =
    [vectorStatement|
      select enumlabel :: text
      from pg_enum
      where enumtypid = $1 :: int4
      order by enumsortorder
    |]
      & lmap (.oid)
