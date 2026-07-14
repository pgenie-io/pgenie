-- | Translation of sessions data structures to logic data structures.
module Infra.Adapters.Analyser.Embeddings.Sessions
  ( adaptQuery,
  )
where

import Data.Vector qualified as Vector
import GenBridge.Model.Input qualified as Gen.Input
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
    pure $ Gen.Input.PrimitiveScalar (adaptPrimitive prim)
  Sessions.CompositeScalar comp -> do
    name <- textToName comp.name
    pure $
      Gen.Input.CustomScalar
        Gen.Input.CustomTypeRef
          { name,
            pgSchema = comp.schemaName,
            pgName = comp.name,
            index = 0
          }
  Sessions.EnumScalar enum -> do
    name <- textToName enum.name
    pure $
      Gen.Input.CustomScalar
        Gen.Input.CustomTypeRef
          { name,
            pgSchema = enum.schemaName,
            pgName = enum.name,
            index = 0
          }

adaptPrimitive :: Sessions.Primitive -> Gen.Input.Primitive
adaptPrimitive = \case
  Sessions.BoolPrimitive -> Gen.Input.BoolPrimitive
  Sessions.ByteaPrimitive -> Gen.Input.ByteaPrimitive
  Sessions.CharPrimitive -> Gen.Input.CharPrimitive
  Sessions.CidrPrimitive -> Gen.Input.CidrPrimitive
  Sessions.DatePrimitive -> Gen.Input.DatePrimitive
  Sessions.DatemultirangePrimitive -> Gen.Input.DatemultirangePrimitive
  Sessions.DaterangePrimitive -> Gen.Input.DaterangePrimitive
  Sessions.Float4Primitive -> Gen.Input.Float4Primitive
  Sessions.Float8Primitive -> Gen.Input.Float8Primitive
  Sessions.InetPrimitive -> Gen.Input.InetPrimitive
  Sessions.Int2Primitive -> Gen.Input.Int2Primitive
  Sessions.Int4Primitive -> Gen.Input.Int4Primitive
  Sessions.Int4multirangePrimitive -> Gen.Input.Int4multirangePrimitive
  Sessions.Int4rangePrimitive -> Gen.Input.Int4rangePrimitive
  Sessions.Int8Primitive -> Gen.Input.Int8Primitive
  Sessions.Int8multirangePrimitive -> Gen.Input.Int8multirangePrimitive
  Sessions.Int8rangePrimitive -> Gen.Input.Int8rangePrimitive
  Sessions.IntervalPrimitive -> Gen.Input.IntervalPrimitive
  Sessions.JsonPrimitive -> Gen.Input.JsonPrimitive
  Sessions.JsonbPrimitive -> Gen.Input.JsonbPrimitive
  Sessions.MacaddrPrimitive -> Gen.Input.MacaddrPrimitive
  Sessions.Macaddr8Primitive -> Gen.Input.Macaddr8Primitive
  Sessions.MoneyPrimitive -> Gen.Input.MoneyPrimitive
  Sessions.NumericPrimitive -> Gen.Input.NumericPrimitive
  Sessions.NummultirangePrimitive -> Gen.Input.NummultirangePrimitive
  Sessions.NumrangePrimitive -> Gen.Input.NumrangePrimitive
  Sessions.TextPrimitive -> Gen.Input.TextPrimitive
  Sessions.TimePrimitive -> Gen.Input.TimePrimitive
  Sessions.TimestampPrimitive -> Gen.Input.TimestampPrimitive
  Sessions.TimestamptzPrimitive -> Gen.Input.TimestamptzPrimitive
  Sessions.TimetzPrimitive -> Gen.Input.TimetzPrimitive
  Sessions.TsmultirangePrimitive -> Gen.Input.TsmultirangePrimitive
  Sessions.TsrangePrimitive -> Gen.Input.TsrangePrimitive
  Sessions.TstzmultirangePrimitive -> Gen.Input.TstzmultirangePrimitive
  Sessions.TstzrangePrimitive -> Gen.Input.TstzrangePrimitive
  Sessions.UuidPrimitive -> Gen.Input.UuidPrimitive
  Sessions.XmlPrimitive -> Gen.Input.XmlPrimitive
  Sessions.VarcharPrimitive -> Gen.Input.VarcharPrimitive
  Sessions.BpcharPrimitive -> Gen.Input.BpcharPrimitive
  Sessions.BitPrimitive -> Gen.Input.BitPrimitive
  Sessions.VarbitPrimitive -> Gen.Input.VarbitPrimitive
  Sessions.TsvectorPrimitive -> Gen.Input.TsvectorPrimitive
  Sessions.TsqueryPrimitive -> Gen.Input.TsqueryPrimitive
  Sessions.PointPrimitive -> Gen.Input.PointPrimitive
  Sessions.LinePrimitive -> Gen.Input.LinePrimitive
  Sessions.LsegPrimitive -> Gen.Input.LsegPrimitive
  Sessions.BoxPrimitive -> Gen.Input.BoxPrimitive
  Sessions.Box2DPrimitive -> Gen.Input.Box2DPrimitive
  Sessions.Box3DPrimitive -> Gen.Input.Box3DPrimitive
  Sessions.PathPrimitive -> Gen.Input.PathPrimitive
  Sessions.LtreePrimitive -> Gen.Input.LtreePrimitive
  Sessions.PolygonPrimitive -> Gen.Input.PolygonPrimitive
  Sessions.CirclePrimitive -> Gen.Input.CirclePrimitive
  Sessions.PgSnapshotPrimitive -> Gen.Input.PgSnapshotPrimitive
  Sessions.PgLsnPrimitive -> Gen.Input.PgLsnPrimitive
  Sessions.NamePrimitive -> Gen.Input.NamePrimitive
  Sessions.HstorePrimitive -> Gen.Input.HstorePrimitive
  Sessions.CitextPrimitive -> Gen.Input.CitextPrimitive
  Sessions.GeometryPrimitive -> Gen.Input.GeometryPrimitive
  Sessions.GeographyPrimitive -> Gen.Input.GeographyPrimitive
  Sessions.OidPrimitive -> Gen.Input.OidPrimitive

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
          Gen.Input.CompositeCustomTypeDefinition fields
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
          Gen.Input.EnumCustomTypeDefinition variants
      }

adaptEnumVariant :: Text -> Embed Gen.Input.EnumVariant
adaptEnumVariant opt = do
  name <- textToName opt
  pure
    Gen.Input.EnumVariant
      { name,
        pgName = opt
      }
