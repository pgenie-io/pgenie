{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module Modeller.Procedures.ResolveTypeByOid where

import Base.Prelude hiding (Enum)
import HasqlDev
import Modeller.Models.Domain
import Modeller.Procedures.ResolveTypeByOid.Statements qualified as Statements

effect ::
  ( RunsStatement m,
    MonadError Error m
  ) =>
  Word32 ->
  m Type
effect oid = do
  case staticOidType oid of
    Just type_ -> return type_
    Nothing -> do
      res <- runStatementByParams Statements.SelectTypeParams {oid = fromIntegral oid}
      case res of
        Nothing -> error ("Unknown type OID: " <> show oid)
        Just type_ ->
          case type_.elementTypeOid of
            0 -> case type_.type_ of
              "b" -> throwError $ UnsupportedFeature "Base type" ""
              _ -> error "TODO"
            _ -> arrayTypeByElementType <$> effect (fromIntegral type_.elementTypeOid)

arrayTypeByElementType :: Type -> Type
arrayTypeByElementType =
  ArrayType . \case
    ArrayType a -> Array (succ a.dimensions) a.element
    PrimitiveType a -> Array 1 (PrimitiveArrayElement a)
    CompositeType a -> Array 1 (CompositeArrayElement a)
    EnumType a -> Array 1 (EnumArrayElement a)

staticOidType :: Word32 -> Maybe Type
staticOidType = \case
  17 -> Just $ PrimitiveType $ ByteaPrimitive
  18 -> Just $ PrimitiveType $ CharPrimitive
  20 -> Just $ PrimitiveType $ Int8Primitive
  21 -> Just $ PrimitiveType $ Int2Primitive
  23 -> Just $ PrimitiveType $ Int4Primitive
  25 -> Just $ PrimitiveType $ TextPrimitive
  114 -> Just $ PrimitiveType $ JsonPrimitive
  142 -> Just $ PrimitiveType $ XmlPrimitive
  700 -> Just $ PrimitiveType $ Float4Primitive
  701 -> Just $ PrimitiveType $ Float8Primitive
  790 -> Just $ PrimitiveType $ MoneyPrimitive
  829 -> Just $ PrimitiveType $ MacaddrPrimitive
  869 -> Just $ PrimitiveType $ InetPrimitive
  650 -> Just $ PrimitiveType $ CidrPrimitive
  774 -> Just $ PrimitiveType $ Macaddr8Primitive
  1082 -> Just $ PrimitiveType $ DatePrimitive
  1083 -> Just $ PrimitiveType $ TimePrimitive
  1114 -> Just $ PrimitiveType $ TimestampPrimitive
  1184 -> Just $ PrimitiveType $ TimestamptzPrimitive
  1186 -> Just $ PrimitiveType $ IntervalPrimitive
  1266 -> Just $ PrimitiveType $ TimetzPrimitive
  1700 -> Just $ PrimitiveType $ NumericPrimitive
  2950 -> Just $ PrimitiveType $ UuidPrimitive
  3802 -> Just $ PrimitiveType $ JsonbPrimitive
  16 -> Just $ PrimitiveType $ BoolPrimitive
  3904 -> Just $ PrimitiveType $ Int4rangePrimitive
  3910 -> Just $ PrimitiveType $ TstzrangePrimitive
  3906 -> Just $ PrimitiveType $ NumrangePrimitive
  3926 -> Just $ PrimitiveType $ Int8rangePrimitive
  3908 -> Just $ PrimitiveType $ TsrangePrimitive
  3912 -> Just $ PrimitiveType $ DaterangePrimitive
  4451 -> Just $ PrimitiveType $ Int4multirangePrimitive
  4532 -> Just $ PrimitiveType $ NummultirangePrimitive
  4533 -> Just $ PrimitiveType $ TsmultirangePrimitive
  4534 -> Just $ PrimitiveType $ TstzmultirangePrimitive
  4535 -> Just $ PrimitiveType $ DatemultirangePrimitive
  4536 -> Just $ PrimitiveType $ Int8multirangePrimitive
  1001 -> primitiveArray ByteaPrimitive
  1002 -> primitiveArray CharPrimitive
  1016 -> primitiveArray Int8Primitive
  1005 -> primitiveArray Int2Primitive
  1007 -> primitiveArray Int4Primitive
  1009 -> primitiveArray TextPrimitive
  199 -> primitiveArray JsonPrimitive
  143 -> primitiveArray XmlPrimitive
  1021 -> primitiveArray Float4Primitive
  1022 -> primitiveArray Float8Primitive
  791 -> primitiveArray MoneyPrimitive
  1040 -> primitiveArray MacaddrPrimitive
  1041 -> primitiveArray InetPrimitive
  651 -> primitiveArray CidrPrimitive
  775 -> primitiveArray Macaddr8Primitive
  1182 -> primitiveArray DatePrimitive
  1183 -> primitiveArray TimePrimitive
  1115 -> primitiveArray TimestampPrimitive
  1185 -> primitiveArray TimestamptzPrimitive
  1187 -> primitiveArray IntervalPrimitive
  1270 -> primitiveArray TimetzPrimitive
  1231 -> primitiveArray NumericPrimitive
  2951 -> primitiveArray UuidPrimitive
  3807 -> primitiveArray JsonbPrimitive
  1000 -> primitiveArray BoolPrimitive
  3905 -> primitiveArray Int4rangePrimitive
  3911 -> primitiveArray TstzrangePrimitive
  3907 -> primitiveArray NumrangePrimitive
  3927 -> primitiveArray Int8rangePrimitive
  3909 -> primitiveArray TsrangePrimitive
  3913 -> primitiveArray DaterangePrimitive
  6150 -> primitiveArray Int4multirangePrimitive
  6151 -> primitiveArray NummultirangePrimitive
  6152 -> primitiveArray TsmultirangePrimitive
  6153 -> primitiveArray TstzmultirangePrimitive
  6155 -> primitiveArray DatemultirangePrimitive
  6157 -> primitiveArray Int8multirangePrimitive
  _ -> Nothing
  where
    primitiveArray =
      Just . ArrayType . Array 1 . PrimitiveArrayElement
