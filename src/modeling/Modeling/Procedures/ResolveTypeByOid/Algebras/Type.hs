module Modeling.Procedures.ResolveTypeByOid.Algebras.Type where

import Base.Prelude
import Modeling.Domain

maybeFromStandardOid :: Int32 -> Maybe Type
maybeFromStandardOid = \case
  17 -> scalar ByteaPrimitive
  18 -> scalar CharPrimitive
  20 -> scalar Int8Primitive
  21 -> scalar Int2Primitive
  23 -> scalar Int4Primitive
  25 -> scalar TextPrimitive
  114 -> scalar JsonPrimitive
  142 -> scalar XmlPrimitive
  700 -> scalar Float4Primitive
  701 -> scalar Float8Primitive
  790 -> scalar MoneyPrimitive
  829 -> scalar MacaddrPrimitive
  869 -> scalar InetPrimitive
  650 -> scalar CidrPrimitive
  774 -> scalar Macaddr8Primitive
  1082 -> scalar DatePrimitive
  1083 -> scalar TimePrimitive
  1114 -> scalar TimestampPrimitive
  1184 -> scalar TimestamptzPrimitive
  1186 -> scalar IntervalPrimitive
  1266 -> scalar TimetzPrimitive
  1700 -> scalar NumericPrimitive
  2950 -> scalar UuidPrimitive
  3802 -> scalar JsonbPrimitive
  16 -> scalar BoolPrimitive
  3904 -> scalar Int4rangePrimitive
  3910 -> scalar TstzrangePrimitive
  3906 -> scalar NumrangePrimitive
  3926 -> scalar Int8rangePrimitive
  3908 -> scalar TsrangePrimitive
  3912 -> scalar DaterangePrimitive
  4451 -> scalar Int4multirangePrimitive
  4532 -> scalar NummultirangePrimitive
  4533 -> scalar TsmultirangePrimitive
  4534 -> scalar TstzmultirangePrimitive
  4535 -> scalar DatemultirangePrimitive
  4536 -> scalar Int8multirangePrimitive
  1001 -> array ByteaPrimitive
  1002 -> array CharPrimitive
  1016 -> array Int8Primitive
  1005 -> array Int2Primitive
  1007 -> array Int4Primitive
  1009 -> array TextPrimitive
  199 -> array JsonPrimitive
  143 -> array XmlPrimitive
  1021 -> array Float4Primitive
  1022 -> array Float8Primitive
  791 -> array MoneyPrimitive
  1040 -> array MacaddrPrimitive
  1041 -> array InetPrimitive
  651 -> array CidrPrimitive
  775 -> array Macaddr8Primitive
  1182 -> array DatePrimitive
  1183 -> array TimePrimitive
  1115 -> array TimestampPrimitive
  1185 -> array TimestamptzPrimitive
  1187 -> array IntervalPrimitive
  1270 -> array TimetzPrimitive
  1231 -> array NumericPrimitive
  2951 -> array UuidPrimitive
  3807 -> array JsonbPrimitive
  1000 -> array BoolPrimitive
  3905 -> array Int4rangePrimitive
  3911 -> array TstzrangePrimitive
  3907 -> array NumrangePrimitive
  3927 -> array Int8rangePrimitive
  3909 -> array TsrangePrimitive
  3913 -> array DaterangePrimitive
  6150 -> array Int4multirangePrimitive
  6151 -> array NummultirangePrimitive
  6152 -> array TsmultirangePrimitive
  6153 -> array TstzmultirangePrimitive
  6155 -> array DatemultirangePrimitive
  6157 -> array Int8multirangePrimitive
  _ -> Nothing
  where
    scalar = Just . Type 0 . PrimitiveScalar
    array = Just . Type 1 . PrimitiveScalar
