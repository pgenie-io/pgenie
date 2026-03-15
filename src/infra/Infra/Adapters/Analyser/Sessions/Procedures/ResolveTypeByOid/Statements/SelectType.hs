module Infra.Adapters.Analyser.Sessions.Procedures.ResolveTypeByOid.Statements.SelectType where

import Hasql.Mapping.IsStatement
import Hasql.TH
import Utils.Prelude

data SelectTypeParams = SelectTypeParams
  { -- | The object identifier of the type.
    oid :: Int32
  }
  deriving stock (Eq, Show)

type SelectTypeResult = Maybe SelectTypeResultRow

data SelectTypeResultRow = SelectTypeResultRow
  { -- | The name of the type.
    name :: Text,
    -- | The schema name of the type.
    schemaName :: Text,
    -- | The type of the type.
    --
    -- b for a base type, c for a composite type (e.g., a table's row type), d for a domain, e for an enum type, p for a pseudo-type, r for a range type, or m for a multirange type. See also typrelid and typbasetype.
    type_ :: Text,
    -- | The relation id.
    relId :: Int32,
    -- | The element type id.
    elementTypeOid :: Int32
  }

instance IsStatement SelectTypeParams where
  type Result SelectTypeParams = SelectTypeResult
  statement =
    [maybeStatement|
      select typname :: text, nspname :: text, typtype :: text, typrelid :: int4, typelem :: int4
      from pg_type
      join pg_namespace on pg_namespace.oid = pg_type.typnamespace
      where pg_type.oid = $1 :: int4
    |]
      & lmap (.oid)
      & (rmap . fmap) (uncurryN SelectTypeResultRow)
