module Infra.Adapters.Analyser.Sessions.Procedures.ResolveTypeByOid
  ( ResolveTypeByOid (..),
  )
where

import Infra.Adapters.Analyser.Sessions.Algebras.Procedure
import Infra.Adapters.Analyser.Sessions.Domain as Domain
import Infra.Adapters.Analyser.Sessions.Procedures.ResolveTypeByOid.Statements qualified as Statements
import SyntacticClass qualified as Syntactic
import Utils.Prelude hiding (Enum)

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
              Nothing ->
                crash
                  ["Unknown type OID: "]
                  [ ("oid", Syntactic.toText oid)
                  ]
              Just type_ -> pure type_
            inContext ["type:", Syntactic.toTextBuilder type_.name] do
              case type_.elementTypeOid of
                0 -> case type_.type_ of
                  "b" -> crash ["Base types are not supported"] []
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
                    pure (Type 0 (CompositeScalar (Composite type_.schemaName type_.name fields)))
                  "d" -> crash ["Domain types are not supported yet"] []
                  "e" -> do
                    labels <- runStatementByParams Statements.SelectEnumLabelsParams {oid}
                    pure (Type 0 (EnumScalar (Enum type_.schemaName type_.name labels)))
                  "p" -> crash ["Pseudo types are not supported yet"] []
                  "r" -> crash ["Range types are not supported yet"] []
                  "m" -> crash ["Multirange types are not supported yet"] []
                  _ ->
                    crash
                      ["Unexpected tag"]
                      [("tag", Syntactic.toText type_.type_)]
                _ -> inContext ["element"] do
                  elementType <- go (fromIntegral type_.elementTypeOid)
                  if elementType.dimensionality == 0
                    then pure elementType {dimensionality = 1}
                    else do
                      warn ["Dimensionality is already set"]
                      pure elementType

-- | Table of static associations between OIDs and types.
typeOidTable :: [(Int32, Type)]
typeOidTable =
  [ (17, Type 0 (PrimitiveScalar ByteaPrimitive)),
    (18, Type 0 (PrimitiveScalar CharPrimitive)),
    (20, Type 0 (PrimitiveScalar Int8Primitive)),
    (21, Type 0 (PrimitiveScalar Int2Primitive)),
    (23, Type 0 (PrimitiveScalar Int4Primitive)),
    (25, Type 0 (PrimitiveScalar TextPrimitive)),
    (114, Type 0 (PrimitiveScalar JsonPrimitive)),
    (142, Type 0 (PrimitiveScalar XmlPrimitive)),
    (700, Type 0 (PrimitiveScalar Float4Primitive)),
    (701, Type 0 (PrimitiveScalar Float8Primitive)),
    (790, Type 0 (PrimitiveScalar MoneyPrimitive)),
    (829, Type 0 (PrimitiveScalar MacaddrPrimitive)),
    (869, Type 0 (PrimitiveScalar InetPrimitive)),
    (650, Type 0 (PrimitiveScalar CidrPrimitive)),
    (774, Type 0 (PrimitiveScalar Macaddr8Primitive)),
    (1082, Type 0 (PrimitiveScalar DatePrimitive)),
    (1083, Type 0 (PrimitiveScalar TimePrimitive)),
    (1114, Type 0 (PrimitiveScalar TimestampPrimitive)),
    (1184, Type 0 (PrimitiveScalar TimestamptzPrimitive)),
    (1186, Type 0 (PrimitiveScalar IntervalPrimitive)),
    (1266, Type 0 (PrimitiveScalar TimetzPrimitive)),
    (1700, Type 0 (PrimitiveScalar NumericPrimitive)),
    (2950, Type 0 (PrimitiveScalar UuidPrimitive)),
    (3802, Type 0 (PrimitiveScalar JsonbPrimitive)),
    (16, Type 0 (PrimitiveScalar BoolPrimitive)),
    (3904, Type 0 (PrimitiveScalar Int4rangePrimitive)),
    (3910, Type 0 (PrimitiveScalar TstzrangePrimitive)),
    (3906, Type 0 (PrimitiveScalar NumrangePrimitive)),
    (3926, Type 0 (PrimitiveScalar Int8rangePrimitive)),
    (3908, Type 0 (PrimitiveScalar TsrangePrimitive)),
    (3912, Type 0 (PrimitiveScalar DaterangePrimitive)),
    (4451, Type 0 (PrimitiveScalar Int4multirangePrimitive)),
    (4532, Type 0 (PrimitiveScalar NummultirangePrimitive)),
    (4533, Type 0 (PrimitiveScalar TsmultirangePrimitive)),
    (4534, Type 0 (PrimitiveScalar TstzmultirangePrimitive)),
    (4535, Type 0 (PrimitiveScalar DatemultirangePrimitive)),
    (4536, Type 0 (PrimitiveScalar Int8multirangePrimitive)),
    (1001, Type 1 (PrimitiveScalar ByteaPrimitive)),
    (1002, Type 1 (PrimitiveScalar CharPrimitive)),
    (1016, Type 1 (PrimitiveScalar Int8Primitive)),
    (1005, Type 1 (PrimitiveScalar Int2Primitive)),
    (1007, Type 1 (PrimitiveScalar Int4Primitive)),
    (1009, Type 1 (PrimitiveScalar TextPrimitive)),
    (199, Type 1 (PrimitiveScalar JsonPrimitive)),
    (143, Type 1 (PrimitiveScalar XmlPrimitive)),
    (1021, Type 1 (PrimitiveScalar Float4Primitive)),
    (1022, Type 1 (PrimitiveScalar Float8Primitive)),
    (791, Type 1 (PrimitiveScalar MoneyPrimitive)),
    (1040, Type 1 (PrimitiveScalar MacaddrPrimitive)),
    (1041, Type 1 (PrimitiveScalar InetPrimitive)),
    (651, Type 1 (PrimitiveScalar CidrPrimitive)),
    (775, Type 1 (PrimitiveScalar Macaddr8Primitive)),
    (1182, Type 1 (PrimitiveScalar DatePrimitive)),
    (1183, Type 1 (PrimitiveScalar TimePrimitive)),
    (1115, Type 1 (PrimitiveScalar TimestampPrimitive)),
    (1185, Type 1 (PrimitiveScalar TimestamptzPrimitive)),
    (1187, Type 1 (PrimitiveScalar IntervalPrimitive)),
    (1270, Type 1 (PrimitiveScalar TimetzPrimitive)),
    (1231, Type 1 (PrimitiveScalar NumericPrimitive)),
    (2951, Type 1 (PrimitiveScalar UuidPrimitive)),
    (3807, Type 1 (PrimitiveScalar JsonbPrimitive)),
    (1000, Type 1 (PrimitiveScalar BoolPrimitive)),
    (3905, Type 1 (PrimitiveScalar Int4rangePrimitive)),
    (3911, Type 1 (PrimitiveScalar TstzrangePrimitive)),
    (3907, Type 1 (PrimitiveScalar NumrangePrimitive)),
    (3927, Type 1 (PrimitiveScalar Int8rangePrimitive)),
    (3909, Type 1 (PrimitiveScalar TsrangePrimitive)),
    (3913, Type 1 (PrimitiveScalar DaterangePrimitive)),
    (6150, Type 1 (PrimitiveScalar Int4multirangePrimitive)),
    (6151, Type 1 (PrimitiveScalar NummultirangePrimitive)),
    (6152, Type 1 (PrimitiveScalar TsmultirangePrimitive)),
    (6153, Type 1 (PrimitiveScalar TstzmultirangePrimitive)),
    (6155, Type 1 (PrimitiveScalar DatemultirangePrimitive)),
    (6157, Type 1 (PrimitiveScalar Int8multirangePrimitive))
  ]
