-- | Domain types and port for inferring the types of a SQL query's parameters
-- and result columns.
module Logic.Domain.QueryAnalysis
  ( InferredQueryTypes (..),
    InferredParam (..),
  )
where

import Gen qualified
import Utils.Prelude

-- | Inferred parameter and result types for a query.
data InferredQueryTypes = InferredQueryTypes
  { params :: [InferredParam],
    resultColumns :: [Gen.Member],
    mentionedCustomTypes :: [Gen.CustomType]
  }
  deriving stock (Eq, Show)

-- | A single inferred query parameter.
data InferredParam = InferredParam
  { isNullable :: Bool,
    type_ :: Gen.Value
  }
  deriving stock (Eq, Show)
