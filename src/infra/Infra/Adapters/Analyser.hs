module Infra.Adapters.Analyser
  ( Device,
    Error,
    scope,
  )
where

import Analysis qualified
import Base.Prelude hiding (Enum)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Fx
import Hasql.Connection.Settings qualified
import Hasql.Pool qualified
import Hasql.Pool.Config qualified
import Hasql.Session qualified
import HasqlDev qualified
import Logic qualified
import Logic.Name qualified as Name
import PGenieGen.Model.Input qualified as Gen.Input
import TestcontainersFx.Scope qualified
import TestcontainersPostgresql qualified

newtype Device = Device Hasql.Pool.Pool

type Error = Logic.Error

adaptPoolUsageError :: Hasql.Pool.UsageError -> Error
adaptPoolUsageError err =
  Logic.Error
    { path = ["db"],
      message = Text.pack (show err),
      suggestion = Nothing,
      details = []
    }

adaptTestcontainersError :: SomeException -> Error
adaptTestcontainersError err =
  Logic.Error
    { path = ["testcontainers"],
      message = Text.pack (displayException err),
      suggestion = Nothing,
      details = []
    }

scope :: Fx.Scope Error Device
scope = do
  (host, port) <-
    first
      adaptTestcontainersError
      ( TestcontainersFx.Scope.testContainer
          ( TestcontainersPostgresql.setup
              TestcontainersPostgresql.Config
                { tagName = "postgres:18",
                  auth = TestcontainersPostgresql.TrustAuth,
                  forwardLogs = False
                }
          )
      )

  pool <-
    acquire
      ( runTotalIO
          ( \() ->
              Hasql.Pool.acquire
                ( Hasql.Pool.Config.settings
                    [ Hasql.Pool.Config.size 100,
                      Hasql.Pool.Config.staticConnectionSettings
                        (Hasql.Connection.Settings.hostAndPort host port)
                    ]
                )
          )
      )

  registerRelease
    ( runTotalIO
        ( \() ->
            Hasql.Pool.release pool
        )
    )

  pure (Device pool)

instance HasqlDev.RunsSession (Fx Device Error) where
  runSession session =
    runPartialIO \(Device pool) ->
      first adaptPoolUsageError <$> Hasql.Pool.use pool session

instance Logic.DbOps (Fx Device Error) where
  executeMigration migrationText =
    HasqlDev.runSession (Hasql.Session.script migrationText)

  inferQueryTypes sql =
    HasqlDev.runSession (Analysis.inferTypes sql) >>= \case
      Left err ->
        throwError (adaptAnalysisError err)
      Right (query, warnings) ->
        pure
          ( adaptQuery query,
            map adaptAnalysisError warnings
          )
    where
      adaptAnalysisError :: Analysis.Error -> Error
      adaptAnalysisError err =
        Logic.Error
          { path = err.location,
            message = err.reason,
            suggestion = Nothing,
            details = []
          }

      adaptQuery :: Analysis.Query -> Logic.InferredQueryTypes
      adaptQuery query =
        let params =
              fmap adaptParam (Vector.toList query.params)
            resultColumns =
              map adaptResultColumn (Vector.toList query.resultColumns)
            mentionedCustomTypes =
              collectCustomTypes query
         in Logic.InferredQueryTypes {params, resultColumns, mentionedCustomTypes}

      adaptParam :: Analysis.Param -> Logic.InferredParam
      adaptParam param =
        Logic.InferredParam
          { isNullable = param.nullable,
            type_ = adaptType param.type_
          }

      adaptResultColumn :: Analysis.ResultColumn -> Gen.Input.Member
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

      adaptType :: Analysis.Type -> Gen.Input.Value
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

      adaptScalar :: Analysis.Scalar -> Gen.Input.Scalar
      adaptScalar = \case
        Analysis.PrimitiveScalar prim ->
          Gen.Input.ScalarPrimitive (adaptPrimitive prim)
        Analysis.CompositeScalar comp ->
          Gen.Input.ScalarCustom (textToName comp.name)
        Analysis.EnumScalar enum ->
          Gen.Input.ScalarCustom (textToName enum.name)

      adaptPrimitive :: Analysis.Primitive -> Gen.Input.Primitive
      adaptPrimitive = \case
        Analysis.BoolPrimitive -> Gen.Input.PrimitiveBool
        Analysis.ByteaPrimitive -> Gen.Input.PrimitiveBytea
        Analysis.CharPrimitive -> Gen.Input.PrimitiveChar
        Analysis.CidrPrimitive -> Gen.Input.PrimitiveCidr
        Analysis.DatePrimitive -> Gen.Input.PrimitiveDate
        Analysis.DatemultirangePrimitive -> Gen.Input.PrimitiveDatemultirange
        Analysis.DaterangePrimitive -> Gen.Input.PrimitiveDaterange
        Analysis.Float4Primitive -> Gen.Input.PrimitiveFloat4
        Analysis.Float8Primitive -> Gen.Input.PrimitiveFloat8
        Analysis.InetPrimitive -> Gen.Input.PrimitiveInet
        Analysis.Int2Primitive -> Gen.Input.PrimitiveInt2
        Analysis.Int4Primitive -> Gen.Input.PrimitiveInt4
        Analysis.Int4multirangePrimitive -> Gen.Input.PrimitiveInt4multirange
        Analysis.Int4rangePrimitive -> Gen.Input.PrimitiveInt4range
        Analysis.Int8Primitive -> Gen.Input.PrimitiveInt8
        Analysis.Int8multirangePrimitive -> Gen.Input.PrimitiveInt8multirange
        Analysis.Int8rangePrimitive -> Gen.Input.PrimitiveInt8range
        Analysis.IntervalPrimitive -> Gen.Input.PrimitiveInterval
        Analysis.JsonPrimitive -> Gen.Input.PrimitiveJson
        Analysis.JsonbPrimitive -> Gen.Input.PrimitiveJsonb
        Analysis.MacaddrPrimitive -> Gen.Input.PrimitiveMacaddr
        Analysis.Macaddr8Primitive -> Gen.Input.PrimitiveMacaddr8
        Analysis.MoneyPrimitive -> Gen.Input.PrimitiveMoney
        Analysis.NumericPrimitive -> Gen.Input.PrimitiveNumeric
        Analysis.NummultirangePrimitive -> Gen.Input.PrimitiveNummultirange
        Analysis.NumrangePrimitive -> Gen.Input.PrimitiveNumrange
        Analysis.TextPrimitive -> Gen.Input.PrimitiveText
        Analysis.TimePrimitive -> Gen.Input.PrimitiveTime
        Analysis.TimestampPrimitive -> Gen.Input.PrimitiveTimestamp
        Analysis.TimestamptzPrimitive -> Gen.Input.PrimitiveTimestamptz
        Analysis.TimetzPrimitive -> Gen.Input.PrimitiveTimetz
        Analysis.TsmultirangePrimitive -> Gen.Input.PrimitiveTsmultirange
        Analysis.TsrangePrimitive -> Gen.Input.PrimitiveTsrange
        Analysis.TstzmultirangePrimitive -> Gen.Input.PrimitiveTstzmultirange
        Analysis.TstzrangePrimitive -> Gen.Input.PrimitiveTstzrange
        Analysis.UuidPrimitive -> Gen.Input.PrimitiveUuid
        Analysis.XmlPrimitive -> Gen.Input.PrimitiveXml

      collectCustomTypes :: Analysis.Query -> [Gen.Input.CustomType]
      collectCustomTypes query =
        nubBy
          (\a b -> a.pgName == b.pgName)
          ( concatMap (collectFromType . (.type_)) (Vector.toList query.params)
              <> concatMap (collectFromType . (.type_)) (Vector.toList query.resultColumns)
          )

      collectFromType :: Analysis.Type -> [Gen.Input.CustomType]
      collectFromType type_ =
        collectFromScalar type_.scalar

      collectFromScalar :: Analysis.Scalar -> [Gen.Input.CustomType]
      collectFromScalar = \case
        Analysis.PrimitiveScalar _ -> []
        Analysis.CompositeScalar comp -> adaptComposite comp : concatMap (collectFromType . (.type_)) (Vector.toList comp.fields)
        Analysis.EnumScalar enum -> [adaptEnum enum]

      adaptComposite :: Analysis.Composite -> Gen.Input.CustomType
      adaptComposite comp =
        Gen.Input.CustomType
          { name = textToName comp.name,
            pgSchema = "public",
            pgName = comp.name,
            definition =
              Gen.Input.CustomTypeDefinitionComposite
                (map adaptCompositeField (Vector.toList comp.fields))
          }

      adaptCompositeField :: Analysis.CompositeField -> Gen.Input.Member
      adaptCompositeField field =
        Gen.Input.Member
          { name = textToName field.name,
            pgName = field.name,
            isNullable = False,
            value = adaptType field.type_
          }

      adaptEnum :: Analysis.Enum -> Gen.Input.CustomType
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
