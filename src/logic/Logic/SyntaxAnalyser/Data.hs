module Logic.SyntaxAnalyser.Data where

import Base.Prelude

data RowAmount
  = SpecificRowAmount Int
  | UpToRowAmount Int
  | AnyRowAmount
  deriving stock (Show, Eq)

data QuerySyntaxAnalysis = QuerySyntaxAnalysis
  { affectsRows :: Bool,
    resultRowAmount :: RowAmount
  }
  deriving stock (Show, Eq)
