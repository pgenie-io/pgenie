module Modeller.Statements.SelectType where

import Base.Prelude
import Hasql.TH
import HasqlDev

data SelectTypeParams = SelectTypeParams
  { -- | The object identifier of the type.
    oid :: Int32
  }
  deriving stock (Eq, Show)

type SelectTypeResult = Maybe SelectTypeResultRow

data SelectTypeResultRow = SelectTypeResultRow
  { -- | The name of the type.
    name :: Text,
    -- | The type of the type.
    type_ :: Text,
    -- | The relation id.
    relId :: Int32,
    -- | The element type id.
    elemTypeId :: Int32
  }

instance IsStatementParams SelectTypeParams where
  type StatementResultByParams SelectTypeParams = SelectTypeResult
  statementByParams =
    [maybeStatement|
      select typname :: text, typtype :: text, typrelid :: int4, typelem :: int4
      from pg_type
      where oid = $1 :: int4
    |]
      & lmap (.oid)
      & (rmap . fmap) (uncurryN SelectTypeResultRow)
