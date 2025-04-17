module Modeling.Procedures.ResolveColumn
  ( Params (..),
    Result,
    lifted,
  )
where

import Base.Prelude
import HasqlDev qualified as Hasql
import Modeling.Frameworks.Procedure
import Modeling.Procedures.ResolveColumn.Statements qualified as Statements
import SyntacticClass qualified as Syntactic

data Params = Params
  { relationOid :: Int32,
    attributeNum :: Int32
  }
  deriving stock (Show, Eq)

data Result = Result
  { -- | The name of the attribute.
    name :: Text,
    -- | OID of the type of the attribute.
    typeId :: Int32,
    -- | Number of dimensions of the type.
    nDims :: Int32,
    -- | Whether it's not nullable.
    notNull :: Bool
  }

lifted ::
  ( Hasql.RunsStatement m,
    MonadReader Location m,
    MonadError Error m,
    MonadWriter [Error] m
  ) =>
  Params ->
  m Result
lifted (Params relationOid attributeNum) =
  inContext ["relation:", Syntactic.toTextBuilder relationOid] do
    inContext ["attribute:", Syntactic.toTextBuilder attributeNum] do
      case relationOid of
        0 -> crash ["Column does not have a relation associated"]
        _ -> do
          Hasql.runStatementByParams
            Statements.GetAttributeByRelationAndOffsetParams
              { relationOid,
                attributeNum
              }
            >>= \case
              Nothing -> crash ["Column not found"]
              Just (Statements.GetAttributeByRelationAndOffsetResultRow name typeId nDims notNull) ->
                pure
                  Result
                    { name,
                      typeId,
                      nDims,
                      notNull
                    }
