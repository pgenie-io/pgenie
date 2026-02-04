module Analysis.Procedures.ResolveColumn
  ( ResolveColumn (..),
    ResolveColumnResultRow (..),
  )
where

import Analysis.Frameworks.Procedure
import Analysis.Procedures.ResolveColumn.Statements qualified as Statements
import Base.Prelude
import SyntacticClass qualified as Syntactic

data ResolveColumn = ResolveColumn
  { relationOid :: Int32,
    attributeNum :: Int32
  }
  deriving stock (Show, Eq)

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

instance Procedure ResolveColumn where
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
