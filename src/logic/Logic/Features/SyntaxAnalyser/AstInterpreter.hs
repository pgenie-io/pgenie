{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module Logic.Features.SyntaxAnalyser.AstInterpreter where

import Logic.Features.SyntaxAnalyser.Data
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
  do
    baseRowAmount <- selectClauseRowAmount selectClause
    let rowAmount =
          maybe baseRowAmount (selectLimitRowAmount baseRowAmount) selectLimit
    -- TODO: Determine whether it really does not affect rows,
    -- because some selects may include DML.
    pure $ QuerySyntaxAnalysis False rowAmount

selectClauseRowAmount = \case
  Left simpleSelect ->
    pure $ simpleSelectRowAmount simpleSelect
  Right selectWithParens -> do
    QuerySyntaxAnalysis _ rowAmount <- selectWithParensQuerySyntaxAnalysis selectWithParens
    pure rowAmount

simpleSelectRowAmount = \case
  Ast.ValuesSimpleSelect valuesClause ->
    valuesClauseRowAmount valuesClause
  Ast.NormalSimpleSelect _ Nothing Nothing Nothing _ Nothing _ ->
    SpecificRowAmount 1
  Ast.NormalSimpleSelect _ Nothing Nothing _ _ _ _ ->
    UpToRowAmount 1
  _ ->
    AnyRowAmount

valuesClauseRowAmount =
  SpecificRowAmount . length

insertRestRowAmount = \case
  Ast.SelectInsertRest _ _ selectStmt -> do
    QuerySyntaxAnalysis _ rowAmount <- selectStmtQuerySyntaxAnalysis selectStmt
    return $ rowAmount
  Ast.DefaultValuesInsertRest ->
    return $ SpecificRowAmount 1

selectLimitRowAmount rowAmount = \case
  Ast.LimitOffsetSelectLimit limitClause offsetClause ->
    limitClauseRowAmount (offsetClauseRowAmount rowAmount offsetClause) limitClause
  Ast.OffsetLimitSelectLimit offsetClause limitClause ->
    limitClauseRowAmount (offsetClauseRowAmount rowAmount offsetClause) limitClause
  Ast.LimitSelectLimit limitClause ->
    limitClauseRowAmount rowAmount limitClause
  Ast.OffsetSelectLimit offsetClause ->
    offsetClauseRowAmount rowAmount offsetClause

limitClauseRowAmount rowAmount = \case
  Ast.LimitLimitClause selectLimitValue _ ->
    selectLimitValueRowAmount rowAmount selectLimitValue
  Ast.FetchOnlyLimitClause _ maybeSelectFetchFirstValue _ ->
    maybe
      (applyLimitCount 1 rowAmount)
      (\selectFetchFirstValue -> maybe (atMostRowAmount rowAmount) (`applyLimitCount` rowAmount) (selectFetchFirstValueInt selectFetchFirstValue))
      maybeSelectFetchFirstValue

selectLimitValueRowAmount rowAmount = \case
  Ast.ExprSelectLimitValue aExpr ->
    maybe
      (atMostRowAmount rowAmount)
      (\n -> applyLimitCount n rowAmount)
      (aExprInt aExpr)
  Ast.AllSelectLimitValue ->
    rowAmount

offsetClauseRowAmount rowAmount = \case
  Ast.ExprOffsetClause aExpr ->
    maybe
      (atMostRowAmount rowAmount)
      (\n -> applyOffsetCount n rowAmount)
      (aExprInt aExpr)
  Ast.FetchFirstOffsetClause selectFetchFirstValue _ ->
    maybe
      (atMostRowAmount rowAmount)
      (\n -> applyOffsetCount n rowAmount)
      (selectFetchFirstValueInt selectFetchFirstValue)

applyLimitCount limitCount rowAmount =
  let normalizedLimitCount = max 0 limitCount
   in case rowAmount of
        SpecificRowAmount n ->
          SpecificRowAmount (min n normalizedLimitCount)
        UpToRowAmount n ->
          UpToRowAmount (min n normalizedLimitCount)
        AnyRowAmount ->
          UpToRowAmount normalizedLimitCount

applyOffsetCount offsetCount rowAmount =
  let normalizedOffsetCount = max 0 offsetCount
      offsettedRowAmount n =
        max 0 (n - normalizedOffsetCount)
   in case rowAmount of
        SpecificRowAmount n ->
          SpecificRowAmount (offsettedRowAmount n)
        UpToRowAmount n ->
          case offsettedRowAmount n of
            0 -> SpecificRowAmount 0
            remaining -> UpToRowAmount remaining
        AnyRowAmount ->
          AnyRowAmount

atMostRowAmount = \case
  SpecificRowAmount n -> UpToRowAmount n
  rowAmount -> rowAmount

-- * Int parsing

aExprInt = \case
  Ast.CExprAExpr cExpr -> cExprInt cExpr
  _ -> Nothing

cExprInt = \case
  Ast.AexprConstCExpr aExprConst -> aExprConstInt aExprConst
  _ -> Nothing

aExprConstInt = \case
  Ast.IAexprConst int -> Just (fromIntegral int)
  _ -> Nothing

selectFetchFirstValueInt = \case
  Ast.ExprSelectFetchFirstValue cExpr -> cExprInt cExpr
  Ast.NumSelectFetchFirstValue _ number ->
    case number of
      Left int -> Just (fromIntegral int)
      Right _ -> Nothing
