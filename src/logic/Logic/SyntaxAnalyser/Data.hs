module Logic.SyntaxAnalyser.Data where

import Base.Prelude
import PostgresqlSyntax.Ast qualified as Ast

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
