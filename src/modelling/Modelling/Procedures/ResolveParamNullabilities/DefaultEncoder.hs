module Modelling.Procedures.ResolveParamNullabilities.DefaultEncoder
  ( fromType,
  )
where

import Base.Prelude hiding (Enum, fromEnum)
import Data.Vector qualified as Vector
import Hasql.Encoders qualified as Encoders
import Modelling.Domain
import Modelling.Procedures.ResolveParamNullabilities.DefaultEncoder.DefaultValues qualified as DefaultValues

fromType :: Type -> Maybe (Encoders.Value ())
fromType (Type dimensionality scalar) = do
  elemEncoder <- fromScalar scalar
  case dimensionality of
    0 -> Nothing
    1 ->
      Just
        let encoder =
              Encoders.array
                . Encoders.dimension Vector.foldl'
                . Encoders.element
                . Encoders.nullable
                $ elemEncoder
         in encoder $< mempty
    2 ->
      Just
        let encoder =
              Encoders.array
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.element
                . Encoders.nullable
                $ elemEncoder
         in encoder $< mempty
    3 ->
      Just
        let encoder =
              Encoders.array
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.element
                . Encoders.nullable
                $ elemEncoder
         in encoder $< mempty
    4 ->
      Just
        let encoder =
              Encoders.array
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.element
                . Encoders.nullable
                $ elemEncoder
         in encoder $< mempty
    5 ->
      Just
        let encoder =
              Encoders.array
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.element
                . Encoders.nullable
                $ elemEncoder
         in encoder $< mempty
    6 ->
      Just
        let encoder =
              Encoders.array
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.dimension Vector.foldl'
                . Encoders.element
                . Encoders.nullable
                $ elemEncoder
         in encoder $< mempty
    _ -> Nothing

fromScalar :: Scalar -> Maybe (Encoders.Value ())
fromScalar = \case
  PrimitiveScalar a -> fromPrimitive a
  CompositeScalar a -> fromComposite a
  EnumScalar a -> fromEnum a

fromPrimitive :: Primitive -> Maybe (Encoders.Value ())
fromPrimitive = \case
  BoolPrimitive -> Just $ Encoders.bool $< True
  ByteaPrimitive -> Just $ Encoders.bytea $< mempty
  CharPrimitive -> Just $ Encoders.text $< mempty
  CidrPrimitive -> Just $ Encoders.inet $< DefaultValues.netAddr
  DatePrimitive -> Just $ Encoders.date $< DefaultValues.day
  DatemultirangePrimitive -> Nothing
  DaterangePrimitive -> Nothing
  Float4Primitive -> Just $ Encoders.float4 $< 0
  Float8Primitive -> Just $ Encoders.float8 $< 0
  InetPrimitive -> Just $ Encoders.inet $< DefaultValues.netAddr
  Int2Primitive -> Just $ Encoders.int2 $< 0
  Int4Primitive -> Just $ Encoders.int4 $< 0
  Int4multirangePrimitive -> Nothing
  Int4rangePrimitive -> Nothing
  Int8Primitive -> Just $ Encoders.int8 $< 0
  Int8multirangePrimitive -> Nothing
  Int8rangePrimitive -> Nothing
  IntervalPrimitive -> Just $ Encoders.interval $< 0
  JsonPrimitive -> Just $ Encoders.json $< DefaultValues.json
  JsonbPrimitive -> Just $ Encoders.jsonb $< DefaultValues.json
  MacaddrPrimitive -> Nothing
  Macaddr8Primitive -> Nothing
  MoneyPrimitive -> Nothing
  NumericPrimitive -> Just $ Encoders.numeric $< 0
  NummultirangePrimitive -> Nothing
  NumrangePrimitive -> Nothing
  TextPrimitive -> Just $ Encoders.text $< ""
  TimePrimitive -> Just $ Encoders.time $< read "00:00:00"
  TimestampPrimitive -> Just $ Encoders.timestamp $< read "2000-01-01 00:00:00"
  TimestamptzPrimitive -> Just $ Encoders.timestamptz $< read "2000-01-01 00:00:00"
  TimetzPrimitive -> Just $ Encoders.timetz $< DefaultValues.timetz
  TsmultirangePrimitive -> Nothing
  TsrangePrimitive -> Nothing
  TstzmultirangePrimitive -> Nothing
  TstzrangePrimitive -> Nothing
  UuidPrimitive -> Just $ Encoders.uuid $< read "00000000-0000-0000-0000-000000000000"
  XmlPrimitive -> Nothing

fromComposite :: Composite -> Maybe (Encoders.Value ())
fromComposite (Composite _ fields) =
  Encoders.composite
    . fold
    <$> traverse (fmap (contramap Just . Encoders.field . Encoders.nullable) . fromType . (.type_)) fields

fromEnum :: Enum -> Maybe (Encoders.Value ())
fromEnum (Enum _ variants) =
  case Vector.uncons variants of
    Just (variant, _) -> Just $ Encoders.unknownEnum (const variant)
    Nothing -> Nothing
