{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module Logic.SyntaxAnalyser.AstInterpreter where

import Logic.SyntaxAnalyser.Data
import PostgresqlSyntax.Ast qualified as Ast
import Utils.Prelude

preparableStmtQuerySyntaxAnalysis :: Ast.PreparableStmt -> Either Text QuerySyntaxAnalysis
preparableStmtQuerySyntaxAnalysis preparableStmt =
  case preparableStmt of
    Ast.SelectPreparableStmt selectStmt ->
      selectStmtQuerySyntaxAnalysis selectStmt
    Ast.InsertPreparableStmt (Ast.InsertStmt _ _ insertRest _ returningClause) ->
      case returningClause of
        Nothing ->
          Right $ QuerySyntaxAnalysis True (SpecificRowAmount 0)
        Just _ ->
          QuerySyntaxAnalysis True <$> insertRestRowAmount insertRest
    Ast.UpdatePreparableStmt updateStmt ->
      Right $ updateStmtQuerySyntaxAnalysis updateStmt
    Ast.DeletePreparableStmt deleteStmt ->
      Right $ deleteStmtQuerySyntaxAnalysis deleteStmt
    Ast.CallPreparableStmt _ ->
      Right $ QuerySyntaxAnalysis True (SpecificRowAmount 1)

deleteStmtQuerySyntaxAnalysis (Ast.DeleteStmt _ _ _ _ returningClause) =
  QuerySyntaxAnalysis True (returningClauseRowAmount returningClause)

updateStmtQuerySyntaxAnalysis (Ast.UpdateStmt _ _ _ _ _ returningClause) =
  QuerySyntaxAnalysis True (returningClauseRowAmount returningClause)

returningClauseRowAmount =
  maybe (SpecificRowAmount 0) (const AnyRowAmount)

selectStmtQuerySyntaxAnalysis = \case
  Left selectNoParens -> selectNoParensQuerySyntaxAnalysis selectNoParens
  Right selectWithParens -> selectWithParensQuerySyntaxAnalysis selectWithParens

selectWithParensQuerySyntaxAnalysis = \case
  Ast.NoParensSelectWithParens selectNoParens -> selectNoParensQuerySyntaxAnalysis selectNoParens
  Ast.WithParensSelectWithParens selectWithParens -> selectWithParensQuerySyntaxAnalysis selectWithParens

selectNoParensQuerySyntaxAnalysis (Ast.SelectNoParens _ selectClause _ selectLimit _) =
  case selectLimit of
    Just selectLimit ->
      -- TODO: Determine whether it really does not affect rows,
      -- because some selects may include DML.
      QuerySyntaxAnalysis False <$> selectLimitRowAmount selectLimit
    Nothing ->
      selectClauseSyntaxAnalysis selectClause

selectClauseSyntaxAnalysis = \case
  Left simpleSelect -> Right . QuerySyntaxAnalysis False $ case simpleSelect of
    Ast.ValuesSimpleSelect valuesClause -> valuesClauseRowAmount valuesClause
    _ -> AnyRowAmount
  Right selectWithParens -> selectWithParensQuerySyntaxAnalysis selectWithParens

valuesClauseRowAmount =
  SpecificRowAmount . length

insertRestRowAmount = \case
  Ast.SelectInsertRest _ _ selectStmt -> do
    QuerySyntaxAnalysis _ rowAmount <- selectStmtQuerySyntaxAnalysis selectStmt
    return $ rowAmount
  Ast.DefaultValuesInsertRest ->
    return $ SpecificRowAmount 1

selectLimitRowAmount = \case
  Ast.LimitOffsetSelectLimit limitClause _ -> limitClauseRowAmount limitClause
  Ast.OffsetLimitSelectLimit _ limitClause -> limitClauseRowAmount limitClause
  Ast.LimitSelectLimit limitClause -> limitClauseRowAmount limitClause
  _ -> Right AnyRowAmount

limitClauseRowAmount = \case
  Ast.LimitLimitClause selectLimitValue _ -> selectLimitValueRowAmount selectLimitValue
  _ -> Right AnyRowAmount

selectLimitValueRowAmount = \case
  Ast.ExprSelectLimitValue aExpr -> case aExprInt aExpr of
    Right n -> Right $ UpToRowAmount n
    Left err -> Left err
  Ast.AllSelectLimitValue ->
    Right AnyRowAmount

-- * Int parsing

aExprInt = \case
  Ast.CExprAExpr cExpr -> cExprInt cExpr
  _ -> Left "Unparsable aExpr"

cExprInt = \case
  Ast.AexprConstCExpr aExprConst -> aExprConstInt aExprConst
  _ -> Left "Unparsable cExpr"

aExprConstInt = \case
  Ast.IAexprConst int -> Right (fromIntegral int)
  _ -> Left "Unparsable aExprConst"
