module Logic.SyntaxAnalyser
  ( resolveText,
    module Data,
  )
where

import Base.Prelude
import Logic.SyntaxAnalyser.AstInterpreter qualified as AstInterpreter
import Logic.SyntaxAnalyser.Data as Data
import PostgresqlSyntax.Parsing qualified as Parsing

resolveText :: Text -> Either Text QuerySyntaxAnalysis
resolveText sql =
  case Parsing.run (Parsing.inSpace Parsing.preparableStmt) sql of
    Left reason -> Left (onto reason)
    Right ast -> AstInterpreter.preparableStmtQuerySyntaxAnalysis ast
