-- | Domain types and port for inferring the types of a SQL query's parameters
-- and result columns.
module Logic.Features.QueryAnalysis
  ( InferredQueryTypes (..),
    InferredParam (..),
    InfersQueryTypes (..),
  )
where

import Logic.Features.Report (Report (..))
import PGenieGen.Model.Input qualified as Gen.Input
import Utils.Prelude

-- | Inferred parameter and result types for a query.
data InferredQueryTypes = InferredQueryTypes
  { params :: [InferredParam],
    resultColumns :: [Gen.Input.Member],
    mentionedCustomTypes :: [Gen.Input.CustomType]
  }
  deriving stock (Eq, Show)

-- | A single inferred query parameter.
data InferredParam = InferredParam
  { isNullable :: Bool,
    type_ :: Gen.Input.Value
  }
  deriving stock (Eq, Show)

-- | Port for inferring the parameter and result types of a SQL query.
class (MonadError Report m) => InfersQueryTypes m where
  inferQueryTypes :: Text -> m (InferredQueryTypes, [Report])
