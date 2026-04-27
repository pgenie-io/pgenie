-- | Domain types and port for inferring the types of a SQL query's parameters
-- and result columns.
module Logic.Features.QueryAnalysis.Types.QueryAnalysis
  ( InferredQueryTypes (..),
    InferredParam (..),
  )
where

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
