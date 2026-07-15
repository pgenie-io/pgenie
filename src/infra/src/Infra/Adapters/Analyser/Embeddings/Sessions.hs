-- | Translation of sessions data structures to logic data structures.
module Infra.Adapters.Analyser.Embeddings.Sessions
  ( adaptQuery,
  )
where

import Data.Vector qualified as Vector
import GenBridge.Contract qualified as Gen
import Infra.Adapters.Analyser.Sessions qualified as Sessions
import Logic.Domain.Name qualified as Name
import Logic.Domain.QueryAnalysis (InferredParam (..), InferredQueryTypes (..))
import Logic.Domain.Report qualified as Report
import Utils.Prelude

type Embed = Either Report.Report

-- |
-- Translate a resolved 'Sessions.Query' into the logic-layer's
-- 'InferredQueryTypes', converting each mentioned name and collecting the
-- custom (composite/enum) types the query touches. Fails if a column name
-- doesn't form a valid identifier.
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

adaptResultColumn :: Sessions.ResultColumn -> Embed Gen.Member
adaptResultColumn col = do
  name <- textToName col.name
  value <- adaptType col.type_
  pure
    Gen.Member
      { name,
        pgName = col.name,
        isNullable = col.nullable,
        value
      }

textToName :: Text -> Embed Gen.Name
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

adaptType :: Sessions.Type -> Embed Gen.Value
adaptType type_ = do
  scalar <- adaptScalar type_.scalar
  pure
    Gen.Value
      { dimensionality = fromIntegral type_.dimensionality,
        elementIsNullable = type_.dimensionality > 0,
        scalar
      }

adaptScalar :: Sessions.Scalar -> Embed Gen.Scalar
adaptScalar = \case
  Sessions.PrimitiveScalar prim ->
    pure $ Gen.PrimitiveScalar (adaptPrimitive prim)
  Sessions.CompositeScalar comp -> do
    name <- textToName comp.name
    pure $
      Gen.CustomScalar
        Gen.CustomTypeRef
          { name,
            pgSchema = comp.schemaName,
            pgName = comp.name,
            index = 0
          }
  Sessions.EnumScalar enum -> do
    name <- textToName enum.name
    pure $
      Gen.CustomScalar
        Gen.CustomTypeRef
          { name,
            pgSchema = enum.schemaName,
            pgName = enum.name,
            index = 0
          }

adaptPrimitive :: Sessions.Primitive -> Gen.Primitive
adaptPrimitive = \case
  Sessions.BoolPrimitive -> Gen.BoolPrimitive
  Sessions.ByteaPrimitive -> Gen.ByteaPrimitive
  Sessions.CharPrimitive -> Gen.CharPrimitive
  Sessions.CidrPrimitive -> Gen.CidrPrimitive
  Sessions.DatePrimitive -> Gen.DatePrimitive
  Sessions.DatemultirangePrimitive -> Gen.DatemultirangePrimitive
  Sessions.DaterangePrimitive -> Gen.DaterangePrimitive
  Sessions.Float4Primitive -> Gen.Float4Primitive
  Sessions.Float8Primitive -> Gen.Float8Primitive
  Sessions.InetPrimitive -> Gen.InetPrimitive
  Sessions.Int2Primitive -> Gen.Int2Primitive
  Sessions.Int4Primitive -> Gen.Int4Primitive
  Sessions.Int4multirangePrimitive -> Gen.Int4multirangePrimitive
  Sessions.Int4rangePrimitive -> Gen.Int4rangePrimitive
  Sessions.Int8Primitive -> Gen.Int8Primitive
  Sessions.Int8multirangePrimitive -> Gen.Int8multirangePrimitive
  Sessions.Int8rangePrimitive -> Gen.Int8rangePrimitive
  Sessions.IntervalPrimitive -> Gen.IntervalPrimitive
  Sessions.JsonPrimitive -> Gen.JsonPrimitive
  Sessions.JsonbPrimitive -> Gen.JsonbPrimitive
  Sessions.MacaddrPrimitive -> Gen.MacaddrPrimitive
  Sessions.Macaddr8Primitive -> Gen.Macaddr8Primitive
  Sessions.MoneyPrimitive -> Gen.MoneyPrimitive
  Sessions.NumericPrimitive -> Gen.NumericPrimitive
  Sessions.NummultirangePrimitive -> Gen.NummultirangePrimitive
  Sessions.NumrangePrimitive -> Gen.NumrangePrimitive
  Sessions.TextPrimitive -> Gen.TextPrimitive
  Sessions.TimePrimitive -> Gen.TimePrimitive
  Sessions.TimestampPrimitive -> Gen.TimestampPrimitive
  Sessions.TimestamptzPrimitive -> Gen.TimestamptzPrimitive
  Sessions.TimetzPrimitive -> Gen.TimetzPrimitive
  Sessions.TsmultirangePrimitive -> Gen.TsmultirangePrimitive
  Sessions.TsrangePrimitive -> Gen.TsrangePrimitive
  Sessions.TstzmultirangePrimitive -> Gen.TstzmultirangePrimitive
  Sessions.TstzrangePrimitive -> Gen.TstzrangePrimitive
  Sessions.UuidPrimitive -> Gen.UuidPrimitive
  Sessions.XmlPrimitive -> Gen.XmlPrimitive
  Sessions.VarcharPrimitive -> Gen.VarcharPrimitive
  Sessions.BpcharPrimitive -> Gen.BpcharPrimitive
  Sessions.BitPrimitive -> Gen.BitPrimitive
  Sessions.VarbitPrimitive -> Gen.VarbitPrimitive
  Sessions.TsvectorPrimitive -> Gen.TsvectorPrimitive
  Sessions.TsqueryPrimitive -> Gen.TsqueryPrimitive
  Sessions.PointPrimitive -> Gen.PointPrimitive
  Sessions.LinePrimitive -> Gen.LinePrimitive
  Sessions.LsegPrimitive -> Gen.LsegPrimitive
  Sessions.BoxPrimitive -> Gen.BoxPrimitive
  Sessions.Box2DPrimitive -> Gen.Box2DPrimitive
  Sessions.Box3DPrimitive -> Gen.Box3DPrimitive
  Sessions.PathPrimitive -> Gen.PathPrimitive
  Sessions.LtreePrimitive -> Gen.LtreePrimitive
  Sessions.PolygonPrimitive -> Gen.PolygonPrimitive
  Sessions.CirclePrimitive -> Gen.CirclePrimitive
  Sessions.PgSnapshotPrimitive -> Gen.PgSnapshotPrimitive
  Sessions.PgLsnPrimitive -> Gen.PgLsnPrimitive
  Sessions.NamePrimitive -> Gen.NamePrimitive
  Sessions.HstorePrimitive -> Gen.HstorePrimitive
  Sessions.CitextPrimitive -> Gen.CitextPrimitive
  Sessions.GeometryPrimitive -> Gen.GeometryPrimitive
  Sessions.GeographyPrimitive -> Gen.GeographyPrimitive
  Sessions.OidPrimitive -> Gen.OidPrimitive

collectCustomTypes :: Sessions.Query -> Embed [Gen.CustomType]
collectCustomTypes query = do
  paramTypes <- traverse (collectFromType . (.type_)) (Vector.toList query.params)
  resultTypes <- traverse (collectFromType . (.type_)) (Vector.toList query.resultColumns)
  pure $
    nubBy
      (\a b -> a.pgName == b.pgName)
      (concat paramTypes <> concat resultTypes)

collectFromType :: Sessions.Type -> Embed [Gen.CustomType]
collectFromType type_ =
  collectFromScalar type_.scalar

collectFromScalar :: Sessions.Scalar -> Embed [Gen.CustomType]
collectFromScalar = \case
  Sessions.PrimitiveScalar _ -> pure []
  Sessions.CompositeScalar comp -> do
    composite <- adaptComposite comp
    fieldTypes <- traverse (collectFromType . (.type_)) (Vector.toList comp.fields)
    pure $ composite : concat fieldTypes
  Sessions.EnumScalar enum -> do
    enum' <- adaptEnum enum
    pure [enum']

adaptComposite :: Sessions.Composite -> Embed Gen.CustomType
adaptComposite comp = do
  name <- textToName comp.name
  fields <- traverse adaptCompositeField (Vector.toList comp.fields)
  pure
    Gen.CustomType
      { name,
        pgSchema = comp.schemaName,
        pgName = comp.name,
        definition =
          Gen.CompositeCustomTypeDefinition fields
      }

adaptCompositeField :: Sessions.CompositeField -> Embed Gen.Member
adaptCompositeField field = do
  name <- textToName field.name
  value <- adaptType field.type_
  pure
    Gen.Member
      { name,
        pgName = field.name,
        isNullable = True,
        value
      }

adaptEnum :: Sessions.Enum -> Embed Gen.CustomType
adaptEnum enum = do
  name <- textToName enum.name
  variants <- traverse adaptEnumVariant (Vector.toList enum.options)
  pure
    Gen.CustomType
      { name,
        pgSchema = enum.schemaName,
        pgName = enum.name,
        definition =
          Gen.EnumCustomTypeDefinition variants
      }

adaptEnumVariant :: Text -> Embed Gen.EnumVariant
adaptEnumVariant opt = do
  name <- textToName opt
  pure
    Gen.EnumVariant
      { name,
        pgName = opt
      }
