-- | Translation of sessions data structures to logic data structures.
module Infra.Adapters.Analyser.Embeddings.Sessions where

import Data.Vector qualified as Vector
import Infra.Adapters.Analyser.Sessions qualified as Sessions
import Logic.Features.QueryAnalysis (InferredParam (..), InferredQueryTypes (..))
import Logic.Features.Report qualified as Report
import Logic.Features.Name qualified as Name
import PGenieGen.Model.Input qualified as Gen.Input
import Utils.Prelude

type Embed = Either Report.Report

adaptQuery :: Sessions.Query -> Embed InferredQueryTypes
adaptQuery query = do
  params <-
    traverse adaptParam (Vector.toList query.params)
  resultColumns <-
    traverse adaptResultColumn (Vector.toList query.resultColumns)
  mentionedCustomTypes <-
    collectCustomTypes query
  pure InferredQueryTypes {params, resultColumns, mentionedCustomTypes}

adaptParam :: Sessions.Param -> Embed InferredParam
adaptParam param = do
  type_ <- adaptType param.type_
  pure
    InferredParam
      { isNullable = param.nullable,
        type_
      }

adaptResultColumn :: Sessions.ResultColumn -> Embed Gen.Input.Member
adaptResultColumn col = do
  name <- textToName col.name
  value <- adaptType col.type_
  pure
    Gen.Input.Member
      { name,
        pgName = col.name,
        isNullable = col.nullable,
        value
      }

textToName :: Text -> Embed Gen.Input.Name
textToName text =
  case Name.tryFromText text of
    Right name -> pure (Name.toGenName name)
    Left err ->
      Left
        Report.Report
          { path = [],
            message = err,
            suggestion = Nothing,
            details = []
          }

adaptType :: Sessions.Type -> Embed Gen.Input.Value
adaptType type_ = do
  scalar <- adaptScalar type_.scalar
  pure
    Gen.Input.Value
      { arraySettings =
          if type_.dimensionality > 0
            then
              Just
                Gen.Input.ArraySettings
                  { dimensionality = fromIntegral type_.dimensionality,
                    elementIsNullable = True
                  }
            else Nothing,
        scalar
      }

adaptScalar :: Sessions.Scalar -> Embed Gen.Input.Scalar
adaptScalar = \case
  Sessions.PrimitiveScalar prim ->
    pure $ Gen.Input.ScalarPrimitive (adaptPrimitive prim)
  Sessions.CompositeScalar comp ->
    Gen.Input.ScalarCustom <$> textToName comp.name
  Sessions.EnumScalar enum ->
    Gen.Input.ScalarCustom <$> textToName enum.name

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
  Sessions.VarcharPrimitive -> Gen.Input.PrimitiveVarchar
  Sessions.BpcharPrimitive -> Gen.Input.PrimitiveBpchar
  Sessions.BitPrimitive -> Gen.Input.PrimitiveBit
  Sessions.VarbitPrimitive -> Gen.Input.PrimitiveVarbit
  Sessions.TsvectorPrimitive -> Gen.Input.PrimitiveTsvector
  Sessions.TsqueryPrimitive -> Gen.Input.PrimitiveTsquery
  Sessions.PointPrimitive -> Gen.Input.PrimitivePoint
  Sessions.LinePrimitive -> Gen.Input.PrimitiveLine
  Sessions.LsegPrimitive -> Gen.Input.PrimitiveLseg
  Sessions.BoxPrimitive -> Gen.Input.PrimitiveBox
  Sessions.Box2DPrimitive -> Gen.Input.PrimitiveBox2D
  Sessions.Box3DPrimitive -> Gen.Input.PrimitiveBox3D
  Sessions.PathPrimitive -> Gen.Input.PrimitivePath
  Sessions.LtreePrimitive -> Gen.Input.PrimitiveLtree
  Sessions.PolygonPrimitive -> Gen.Input.PrimitivePolygon
  Sessions.CirclePrimitive -> Gen.Input.PrimitiveCircle
  Sessions.PgSnapshotPrimitive -> Gen.Input.PrimitivePgSnapshot
  Sessions.PgLsnPrimitive -> Gen.Input.PrimitivePgLsn
  Sessions.NamePrimitive -> Gen.Input.PrimitiveName
  Sessions.HstorePrimitive -> Gen.Input.PrimitiveHstore
  Sessions.CitextPrimitive -> Gen.Input.PrimitiveCitext
  Sessions.GeometryPrimitive -> Gen.Input.PrimitiveGeometry
  Sessions.GeographyPrimitive -> Gen.Input.PrimitiveGeography
  Sessions.OidPrimitive -> Gen.Input.PrimitiveOid

collectCustomTypes :: Sessions.Query -> Embed [Gen.Input.CustomType]
collectCustomTypes query = do
  paramTypes <- traverse (collectFromType . (.type_)) (Vector.toList query.params)
  resultTypes <- traverse (collectFromType . (.type_)) (Vector.toList query.resultColumns)
  pure $
    nubBy
      (\a b -> a.pgName == b.pgName)
      (concat paramTypes <> concat resultTypes)

collectFromType :: Sessions.Type -> Embed [Gen.Input.CustomType]
collectFromType type_ =
  collectFromScalar type_.scalar

collectFromScalar :: Sessions.Scalar -> Embed [Gen.Input.CustomType]
collectFromScalar = \case
  Sessions.PrimitiveScalar _ -> pure []
  Sessions.CompositeScalar comp -> do
    composite <- adaptComposite comp
    fieldTypes <- traverse (collectFromType . (.type_)) (Vector.toList comp.fields)
    pure $ composite : concat fieldTypes
  Sessions.EnumScalar enum -> do
    enum' <- adaptEnum enum
    pure [enum']

adaptComposite :: Sessions.Composite -> Embed Gen.Input.CustomType
adaptComposite comp = do
  name <- textToName comp.name
  fields <- traverse adaptCompositeField (Vector.toList comp.fields)
  pure
    Gen.Input.CustomType
      { name,
        pgSchema = comp.schemaName,
        pgName = comp.name,
        definition =
          Gen.Input.CustomTypeDefinitionComposite fields
      }

adaptCompositeField :: Sessions.CompositeField -> Embed Gen.Input.Member
adaptCompositeField field = do
  name <- textToName field.name
  value <- adaptType field.type_
  pure
    Gen.Input.Member
      { name,
        pgName = field.name,
        isNullable = True,
        value
      }

adaptEnum :: Sessions.Enum -> Embed Gen.Input.CustomType
adaptEnum enum = do
  name <- textToName enum.name
  variants <- traverse adaptEnumVariant (Vector.toList enum.options)
  pure
    Gen.Input.CustomType
      { name,
        pgSchema = enum.schemaName,
        pgName = enum.name,
        definition =
          Gen.Input.CustomTypeDefinitionEnum variants
      }

adaptEnumVariant :: Text -> Embed Gen.Input.EnumVariant
adaptEnumVariant opt = do
  name <- textToName opt
  pure
    Gen.Input.EnumVariant
      { name,
        pgName = opt
      }
