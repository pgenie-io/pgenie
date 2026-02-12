-- | Translation of sessions data structures to logic data structures.
module Infra.Adapters.Analyser.Translations.Sessions where

import Base.Prelude
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Infra.Adapters.Analyser.Sessions qualified as Sessions
import Logic qualified
import Logic.Name qualified as Name
import PGenieGen.Model.Input qualified as Gen.Input

adaptQuery :: Sessions.Query -> Logic.InferredQueryTypes
adaptQuery query =
  let params =
        fmap adaptParam (Vector.toList query.params)
      resultColumns =
        map adaptResultColumn (Vector.toList query.resultColumns)
      mentionedCustomTypes =
        collectCustomTypes query
   in Logic.InferredQueryTypes {params, resultColumns, mentionedCustomTypes}

adaptParam :: Sessions.Param -> Logic.InferredParam
adaptParam param =
  Logic.InferredParam
    { isNullable = param.nullable,
      type_ = adaptType param.type_
    }

adaptResultColumn :: Sessions.ResultColumn -> Gen.Input.Member
adaptResultColumn col =
  Gen.Input.Member
    { name = textToName col.name,
      pgName = col.name,
      isNullable = col.nullable,
      value = adaptType col.type_
    }

textToName :: Text -> Gen.Input.Name
textToName text =
  case Name.tryFromText text of
    Right name -> Name.toGenName name
    Left _ -> Name.toGenName (fromString (filter isAsciiLower (Text.unpack text)))

adaptType :: Sessions.Type -> Gen.Input.Value
adaptType type_ =
  Gen.Input.Value
    { arraySettings =
        if type_.dimensionality > 0
          then
            Just
              Gen.Input.ArraySettings
                { dimensionality = fromIntegral type_.dimensionality,
                  elementIsNullable = False
                }
          else Nothing,
      scalar = adaptScalar type_.scalar
    }

adaptScalar :: Sessions.Scalar -> Gen.Input.Scalar
adaptScalar = \case
  Sessions.PrimitiveScalar prim ->
    Gen.Input.ScalarPrimitive (adaptPrimitive prim)
  Sessions.CompositeScalar comp ->
    Gen.Input.ScalarCustom (textToName comp.name)
  Sessions.EnumScalar enum ->
    Gen.Input.ScalarCustom (textToName enum.name)

adaptPrimitive :: Sessions.Primitive -> Gen.Input.Primitive
adaptPrimitive = \case
  Sessions.BoolPrimitive -> Gen.Input.PrimitiveBool
  Sessions.ByteaPrimitive -> Gen.Input.PrimitiveBytea
  Sessions.CharPrimitive -> Gen.Input.PrimitiveChar
  Sessions.CidrPrimitive -> Gen.Input.PrimitiveCidr
  Sessions.DatePrimitive -> Gen.Input.PrimitiveDate
  Sessions.DatemultirangePrimitive -> Gen.Input.PrimitiveDatemultirange
  Sessions.DaterangePrimitive -> Gen.Input.PrimitiveDaterange
  Sessions.Float4Primitive -> Gen.Input.PrimitiveFloat4
  Sessions.Float8Primitive -> Gen.Input.PrimitiveFloat8
  Sessions.InetPrimitive -> Gen.Input.PrimitiveInet
  Sessions.Int2Primitive -> Gen.Input.PrimitiveInt2
  Sessions.Int4Primitive -> Gen.Input.PrimitiveInt4
  Sessions.Int4multirangePrimitive -> Gen.Input.PrimitiveInt4multirange
  Sessions.Int4rangePrimitive -> Gen.Input.PrimitiveInt4range
  Sessions.Int8Primitive -> Gen.Input.PrimitiveInt8
  Sessions.Int8multirangePrimitive -> Gen.Input.PrimitiveInt8multirange
  Sessions.Int8rangePrimitive -> Gen.Input.PrimitiveInt8range
  Sessions.IntervalPrimitive -> Gen.Input.PrimitiveInterval
  Sessions.JsonPrimitive -> Gen.Input.PrimitiveJson
  Sessions.JsonbPrimitive -> Gen.Input.PrimitiveJsonb
  Sessions.MacaddrPrimitive -> Gen.Input.PrimitiveMacaddr
  Sessions.Macaddr8Primitive -> Gen.Input.PrimitiveMacaddr8
  Sessions.MoneyPrimitive -> Gen.Input.PrimitiveMoney
  Sessions.NumericPrimitive -> Gen.Input.PrimitiveNumeric
  Sessions.NummultirangePrimitive -> Gen.Input.PrimitiveNummultirange
  Sessions.NumrangePrimitive -> Gen.Input.PrimitiveNumrange
  Sessions.TextPrimitive -> Gen.Input.PrimitiveText
  Sessions.TimePrimitive -> Gen.Input.PrimitiveTime
  Sessions.TimestampPrimitive -> Gen.Input.PrimitiveTimestamp
  Sessions.TimestamptzPrimitive -> Gen.Input.PrimitiveTimestamptz
  Sessions.TimetzPrimitive -> Gen.Input.PrimitiveTimetz
  Sessions.TsmultirangePrimitive -> Gen.Input.PrimitiveTsmultirange
  Sessions.TsrangePrimitive -> Gen.Input.PrimitiveTsrange
  Sessions.TstzmultirangePrimitive -> Gen.Input.PrimitiveTstzmultirange
  Sessions.TstzrangePrimitive -> Gen.Input.PrimitiveTstzrange
  Sessions.UuidPrimitive -> Gen.Input.PrimitiveUuid
  Sessions.XmlPrimitive -> Gen.Input.PrimitiveXml

collectCustomTypes :: Sessions.Query -> [Gen.Input.CustomType]
collectCustomTypes query =
  nubBy
    (\a b -> a.pgName == b.pgName)
    ( concatMap (collectFromType . (.type_)) (Vector.toList query.params)
        <> concatMap (collectFromType . (.type_)) (Vector.toList query.resultColumns)
    )

collectFromType :: Sessions.Type -> [Gen.Input.CustomType]
collectFromType type_ =
  collectFromScalar type_.scalar

collectFromScalar :: Sessions.Scalar -> [Gen.Input.CustomType]
collectFromScalar = \case
  Sessions.PrimitiveScalar _ -> []
  Sessions.CompositeScalar comp -> adaptComposite comp : concatMap (collectFromType . (.type_)) (Vector.toList comp.fields)
  Sessions.EnumScalar enum -> [adaptEnum enum]

adaptComposite :: Sessions.Composite -> Gen.Input.CustomType
adaptComposite comp =
  Gen.Input.CustomType
    { name = textToName comp.name,
      pgSchema = "public",
      pgName = comp.name,
      definition =
        Gen.Input.CustomTypeDefinitionComposite
          (map adaptCompositeField (Vector.toList comp.fields))
    }

adaptCompositeField :: Sessions.CompositeField -> Gen.Input.Member
adaptCompositeField field =
  Gen.Input.Member
    { name = textToName field.name,
      pgName = field.name,
      isNullable = False,
      value = adaptType field.type_
    }

adaptEnum :: Sessions.Enum -> Gen.Input.CustomType
adaptEnum enum =
  Gen.Input.CustomType
    { name = textToName enum.name,
      pgSchema = "public",
      pgName = enum.name,
      definition =
        Gen.Input.CustomTypeDefinitionEnum
          (map adaptEnumVariant (Vector.toList enum.options))
    }

adaptEnumVariant :: Text -> Gen.Input.EnumVariant
adaptEnumVariant opt =
  Gen.Input.EnumVariant
    { name = textToName opt,
      pgName = opt
    }
