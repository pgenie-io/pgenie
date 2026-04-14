module Infra.Adapters.Analyser.Sessions.Procedures.ResolveParamNullabilities.DefaultTextualValue
  ( fromType,
  )
where

import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Infra.Adapters.Analyser.Sessions.Domain
import Utils.Prelude hiding (Enum, fromEnum)

fromType :: Type -> Maybe Text
fromType (Type dimensionality scalar) =
  case dimensionality of
    0 -> fromScalarText scalar
    _ -> Just "{}"

fromScalarText :: Scalar -> Maybe Text
fromScalarText = \case
  PrimitiveScalar primitive -> fromPrimitiveText primitive
  CompositeScalar composite -> fromCompositeText composite
  EnumScalar enum_ -> fromEnumText enum_

fromPrimitiveText :: Primitive -> Maybe Text
fromPrimitiveText = \case
  BoolPrimitive -> Just "true"
  ByteaPrimitive -> Just "\\x00"
  CharPrimitive -> Just "x"
  CidrPrimitive -> Just "192.0.2.0/24"
  DatePrimitive -> Just "2000-01-01"
  DatemultirangePrimitive -> Just "{}"
  DaterangePrimitive -> Just "empty"
  Float4Primitive -> Just "0"
  Float8Primitive -> Just "0"
  InetPrimitive -> Just "192.0.2.0/24"
  Int2Primitive -> Just "0"
  Int4Primitive -> Just "0"
  Int4multirangePrimitive -> Just "{}"
  Int4rangePrimitive -> Just "empty"
  Int8Primitive -> Just "0"
  Int8multirangePrimitive -> Just "{}"
  Int8rangePrimitive -> Just "empty"
  IntervalPrimitive -> Just "0"
  JsonPrimitive -> Just "null"
  JsonbPrimitive -> Just "null"
  MacaddrPrimitive -> Just "08:00:2b:01:02:03"
  Macaddr8Primitive -> Just "08:00:2b:01:02:03:04:05"
  MoneyPrimitive -> Just "0"
  NumericPrimitive -> Just "0"
  NummultirangePrimitive -> Just "{}"
  NumrangePrimitive -> Just "empty"
  TextPrimitive -> Just ""
  TimePrimitive -> Just "00:00:00"
  TimestampPrimitive -> Just "2000-01-01 00:00:00"
  TimestamptzPrimitive -> Just "2000-01-01 00:00:00+00"
  TimetzPrimitive -> Just "00:00:00+00"
  TsmultirangePrimitive -> Just "{}"
  TsrangePrimitive -> Just "empty"
  TstzmultirangePrimitive -> Just "{}"
  TstzrangePrimitive -> Just "empty"
  UuidPrimitive -> Just "00000000-0000-0000-0000-000000000000"
  XmlPrimitive -> Just "<a/>"
  -- varchar and bpchar use text wire encoding
  VarcharPrimitive -> Just ""
  BpcharPrimitive -> Just ""
  -- name uses text wire encoding
  NamePrimitive -> Just ""
  BitPrimitive -> Just "0"
  VarbitPrimitive -> Just "0"
  TsvectorPrimitive -> Just "a"
  TsqueryPrimitive -> Just "a"
  PointPrimitive -> Just "(0,0)"
  LinePrimitive -> Just "{1,0,0}"
  LsegPrimitive -> Just "[(0,0),(1,1)]"
  BoxPrimitive -> Just "(1,1),(0,0)"
  Box2DPrimitive -> Just "BOX(1 2,5 6)"
  Box3DPrimitive -> Just "BOX3D(1 2 3,5 6 5)"
  PathPrimitive -> Just "[(0,0),(1,1)]"
  LtreePrimitive -> Just "Top.Science"
  PolygonPrimitive -> Just "((0,0),(1,0),(1,1),(0,0))"
  CirclePrimitive -> Just "<(0,0),1>"
  PgSnapshotPrimitive -> Just "1:1:"
  PgLsnPrimitive -> Just "0/0"
  HstorePrimitive -> Just "\"a\"=>\"b\""
  CitextPrimitive -> Just ""
  GeometryPrimitive -> Just "SRID=4326;POINT(0 0)"
  GeographyPrimitive -> Just "SRID=4326;POINT(0 0)"
  OidPrimitive -> Just "0"

fromCompositeText :: Composite -> Maybe Text
fromCompositeText (Composite _ _ fields) = do
  fieldTexts <- traverse (\(CompositeField _ fieldType) -> fromType fieldType) fields
  pure ("(" <> Text.intercalate "," (map quoteCompositeField (Vector.toList fieldTexts)) <> ")")
  where
    quoteCompositeField :: Text -> Text
    quoteCompositeField fieldText =
      "\"" <> Text.concatMap escape fieldText <> "\""
      where
        escape = \case
          '"' -> "\\\""
          '\\' -> "\\\\"
          character -> Text.singleton character

fromEnumText :: Enum -> Maybe Text
fromEnumText (Enum _ _ variants) =
  case Vector.uncons variants of
    Just (variant, _) -> Just variant
    Nothing -> Nothing
