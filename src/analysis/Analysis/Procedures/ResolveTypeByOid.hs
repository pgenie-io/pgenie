module Analysis.Procedures.ResolveTypeByOid
  ( ResolveTypeByOid (..),
  )
where

import Analysis.Domain as Domain
import Analysis.Frameworks.Procedure
import Analysis.Procedures.ResolveTypeByOid.Statements qualified as Statements
import Base.Prelude hiding (Enum)
import SyntacticClass qualified as Syntactic
import TextBuilder qualified

data ResolveTypeByOid = ResolveTypeByOid
  { oid :: Int32
  }
  deriving stock (Show, Eq)

instance Procedure ResolveTypeByOid where
  type ProcedureResult ResolveTypeByOid = Type
  runProcedure (ResolveTypeByOid oid) =
    go oid
    where
      go oid =
        case lookup oid typeOidTable of
          Just type_ -> pure type_
          Nothing -> do
            res <- runStatementByParams Statements.SelectTypeParams {oid}
            type_ <- case res of
              Nothing -> crash ["Unknown type OID: ", Syntactic.toTextBuilder oid]
              Just type_ -> pure type_
            inContext ["type:", Syntactic.toTextBuilder type_.name] do
              case type_.elementTypeOid of
                0 -> case type_.type_ of
                  "b" -> crash ["Base types are not supported"]
                  "c" -> do
                    fields <- runStatementByParams Statements.SelectAttributesParams {oid}
                    fields <- forM fields $ \(Statements.SelectAttributesResultRow fieldName fieldTypeOid (fromIntegral -> fieldDims) _) ->
                      inContext ["field:", Syntactic.toTextBuilder fieldName] do
                        fieldType <- go (fromIntegral fieldTypeOid)
                        fieldType <- case fieldDims of
                          0 -> pure fieldType
                          _ -> case fieldType of
                            Type existingDims elementScalar ->
                              if existingDims /= fieldDims
                                then do
                                  warn ["Replacing dimensionality from ", Syntactic.toTextBuilder existingDims, " to ", Syntactic.toTextBuilder fieldDims]
                                  pure (Type fieldDims elementScalar)
                                else pure fieldType
                        pure (CompositeField fieldName fieldType)
                    pure (Type 0 (CompositeScalar (Composite type_.name fields)))
                  "d" -> crash ["Domain types are not supported yet"]
                  "e" -> do
                    labels <- runStatementByParams Statements.SelectEnumLabelsParams {oid}
                    pure (Type 0 (EnumScalar (Enum type_.name labels)))
                  "p" -> crash ["Pseudo types are not supported yet"]
                  "r" -> crash ["Range types are not supported yet"]
                  "m" -> crash ["Multirange types are not supported yet"]
                  _ -> crash ["Unexpected tag: ", TextBuilder.text type_.type_]
                _ -> inContext ["element"] do
                  elementType <- go (fromIntegral type_.elementTypeOid)
                  if elementType.dimensionality == 0
                    then pure elementType {dimensionality = 1}
                    else do
                      warn ["Dimensionality is already set"]
                      pure elementType
