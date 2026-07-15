-- |
-- 'IsProcedure' member that resolves the catalog attribute (name, type,
-- dimensionality, nullability) a query's result column was derived from, so
-- its nullability can reflect the source table's constraint rather than
-- libpq's (always-nullable) type description.
module Infra.Adapters.Analyser.Sessions.Procedures.ResolveColumn
  ( ResolveColumn (..),
    ResolveColumnResultRow (..),
  )
where

import Infra.Adapters.Analyser.Sessions.Algebras.Procedure
import Infra.Adapters.Analyser.Sessions.Procedures.ResolveColumn.Statements qualified as Statements
import SyntacticClass qualified as Syntactic
import Utils.Prelude

-- | The table relation and attribute offset identifying a result column's source.
data ResolveColumn = ResolveColumn
  { relationOid :: Int32,
    attributeNum :: Int32
  }
  deriving stock (Show, Eq)

-- | The resolved source-column attribute, when the relation and offset refer to one.
data ResolveColumnResultRow = ResolveColumnResultRow
  { -- | The name of the attribute.
    name :: Text,
    -- | OID of the type of the attribute.
    typeOid :: Int32,
    -- | Number of dimensions of the type.
    nDims :: Int32,
    -- | Whether it's not nullable.
    notNull :: Bool
  }

instance IsProcedure ResolveColumn where
  type ProcedureResult ResolveColumn = Maybe ResolveColumnResultRow
  runProcedure (ResolveColumn relationOid attributeNum) =
    inContext ["relation:", Syntactic.toTextBuilder relationOid] do
      inContext ["attribute:", Syntactic.toTextBuilder attributeNum] do
        runStatementByParams
          Statements.GetAttributeByRelationAndOffsetParams
            { relationOid,
              attributeNum
            }
          >>= traverse
            ( \(Statements.GetAttributeByRelationAndOffsetResultRow name typeOid nDims notNull) ->
                pure
                  ResolveColumnResultRow
                    { name,
                      typeOid,
                      nDims,
                      notNull
                    }
            )
