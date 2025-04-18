module Modeling.Procedures.ResolveColumn
  ( ResolveColumn (..),
    ResolveColumnResult (..),
  )
where

import Base.Prelude
import Modeling.Frameworks.Procedure
import Modeling.Procedures.ResolveColumn.Statements qualified as Statements
import SyntacticClass qualified as Syntactic

data ResolveColumn = ResolveColumn
  { relationOid :: Int32,
    attributeNum :: Int32
  }
  deriving stock (Show, Eq)

data ResolveColumnResult = ResolveColumnResult
  { -- | The name of the attribute.
    name :: Text,
    -- | OID of the type of the attribute.
    typeId :: Int32,
    -- | Number of dimensions of the type.
    nDims :: Int32,
    -- | Whether it's not nullable.
    notNull :: Bool
  }

instance Procedure ResolveColumn where
  type ProcedureResult ResolveColumn = ResolveColumnResult
  runProcedure (ResolveColumn relationOid attributeNum) =
    inContext ["relation:", Syntactic.toTextBuilder relationOid] do
      inContext ["attribute:", Syntactic.toTextBuilder attributeNum] do
        case relationOid of
          0 -> crash ["Column does not have a relation associated"]
          _ -> do
            runStatementByParams
              Statements.GetAttributeByRelationAndOffsetParams
                { relationOid,
                  attributeNum
                }
              >>= \case
                Nothing -> crash ["Column not found"]
                Just (Statements.GetAttributeByRelationAndOffsetResultRow name typeId nDims notNull) ->
                  pure
                    ResolveColumnResult
                      { name,
                        typeId,
                        nDims,
                        notNull
                      }
