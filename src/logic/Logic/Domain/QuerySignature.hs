-- |
-- Query signature files (@.sig1.pgn.yaml@): serialization, parsing, and
-- validation-and-merge against a query's inferred parameter and result types.
module Logic.Domain.QuerySignature
  ( Signature (..),
    signatureFilePath,
    fromInferred,
    serialize,
    tryParse,
    validateAndMerge,
    applyToQuery,
    spec,
  )
where

import Test.Hspec
import Utils.Prelude hiding (readFile, writeFile)
import AlgebraicPath qualified as Path
import Control.Foldl qualified as Fold
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GenBridge.Model.Input qualified as Gen.Input
import Logic.Domain.Name qualified as Name
import Logic.Domain.Report qualified as Report
import YamlUnscrambler qualified as U

-- * Types

data Signature = Signature
  { idempotent :: Bool,
    parameters :: [(Text, FieldSig)],
    result :: Maybe ResultSig
  }
  deriving stock (Eq, Show)

data FieldSig
  = ScalarFieldSig
      { typeName :: Text,
        notNull :: Bool
      }
  | ArrayFieldSig
      { typeName :: Text,
        notNull :: Bool,
        elementNotNull :: Bool
      }
  deriving stock (Eq, Show)

data ResultSig = ResultSig
  { cardinality :: Cardinality,
    columns :: [(Text, FieldSig)]
  }
  deriving stock (Eq, Show)

data Cardinality
  = CardinalityZeroOrOne
  | CardinalityOne
  | CardinalityMany
  deriving stock (Eq, Show)

-- * Path computation

-- | Compute the signature file path from a query file path.
--
-- >>> signatureFilePath "queries/select_album_by_format.sql"
-- "queries/select_album_by_format.sig1.pgn.yaml"
signatureFilePath :: Path -> Path
signatureFilePath queryPath =
  Path.addExtension "sig1.pgn.yaml" (Path.dropExtensions queryPath)

-- * Conversion from inferred types

-- | Create a signature from inferred query types.
fromInferred :: [Gen.Input.Member] -> Gen.Input.Result -> Signature
fromInferred params result =
  Signature
    { idempotent = False,
      parameters = map memberToFieldEntry params,
      result = resultToSigResult result
    }
  where
    memberToFieldEntry :: Gen.Input.Member -> (Text, FieldSig)
    memberToFieldEntry member =
      ( member.pgName,
        fieldSigFromValue member.value (not member.isNullable)
      )

    resultRowsToResultSig :: Gen.Input.ResultRows -> ResultSig
    resultRowsToResultSig rr =
      ResultSig
        { cardinality = cardinalityFromGenInput rr.cardinality,
          columns = map memberToFieldEntry (toList rr.columns)
        }

    resultToSigResult :: Gen.Input.Result -> Maybe ResultSig
    resultToSigResult = \case
      Gen.Input.RowsResult rows -> Just (resultRowsToResultSig rows)
      Gen.Input.RowsAffectedResult -> Nothing
      Gen.Input.VoidResult -> Nothing

fieldSigFromValue :: Gen.Input.Value -> Bool -> FieldSig
fieldSigFromValue value fieldNotNull =
  case value.arraySettings of
    Just settings ->
      ArrayFieldSig
        { typeName = valueToTypeName value,
          notNull = fieldNotNull,
          elementNotNull = not settings.elementIsNullable
        }
    Nothing ->
      ScalarFieldSig
        { typeName = valueToTypeName value,
          notNull = fieldNotNull
        }

-- * Cardinality conversion

cardinalityFromGenInput :: Gen.Input.ResultRowsCardinality -> Cardinality
cardinalityFromGenInput = \case
  Gen.Input.OptionalResultRowsCardinality -> CardinalityZeroOrOne
  Gen.Input.SingleResultRowsCardinality -> CardinalityOne
  Gen.Input.MultipleResultRowsCardinality -> CardinalityMany

cardinalityToGenInput :: Cardinality -> Gen.Input.ResultRowsCardinality
cardinalityToGenInput = \case
  CardinalityZeroOrOne -> Gen.Input.OptionalResultRowsCardinality
  CardinalityOne -> Gen.Input.SingleResultRowsCardinality
  CardinalityMany -> Gen.Input.MultipleResultRowsCardinality

-- * Value to type name

valueToTypeName :: Gen.Input.Value -> Text
valueToTypeName value =
  let baseName = scalarToTypeName value.scalar
      arraySuffix = case value.arraySettings of
        Nothing -> ""
        Just settings -> Text.replicate (fromIntegral settings.dimensionality) "[]"
   in baseName <> arraySuffix

scalarToTypeName :: Gen.Input.Scalar -> Text
scalarToTypeName = \case
  Gen.Input.PrimitiveScalar prim -> primitiveToTypeName prim
  Gen.Input.CustomScalar name -> genNameToText name

primitiveToTypeName :: Gen.Input.Primitive -> Text
primitiveToTypeName = \case
  Gen.Input.BoolPrimitive -> "bool"
  Gen.Input.ByteaPrimitive -> "bytea"
  Gen.Input.CharPrimitive -> "char"
  Gen.Input.CidrPrimitive -> "cidr"
  Gen.Input.DatePrimitive -> "date"
  Gen.Input.DatemultirangePrimitive -> "datemultirange"
  Gen.Input.DaterangePrimitive -> "daterange"
  Gen.Input.Float4Primitive -> "float4"
  Gen.Input.Float8Primitive -> "float8"
  Gen.Input.InetPrimitive -> "inet"
  Gen.Input.Int2Primitive -> "int2"
  Gen.Input.Int4Primitive -> "int4"
  Gen.Input.Int4multirangePrimitive -> "int4multirange"
  Gen.Input.Int4rangePrimitive -> "int4range"
  Gen.Input.Int8Primitive -> "int8"
  Gen.Input.Int8multirangePrimitive -> "int8multirange"
  Gen.Input.Int8rangePrimitive -> "int8range"
  Gen.Input.IntervalPrimitive -> "interval"
  Gen.Input.JsonPrimitive -> "json"
  Gen.Input.JsonbPrimitive -> "jsonb"
  Gen.Input.MacaddrPrimitive -> "macaddr"
  Gen.Input.Macaddr8Primitive -> "macaddr8"
  Gen.Input.MoneyPrimitive -> "money"
  Gen.Input.NumericPrimitive -> "numeric"
  Gen.Input.NummultirangePrimitive -> "nummultirange"
  Gen.Input.NumrangePrimitive -> "numrange"
  Gen.Input.TextPrimitive -> "text"
  Gen.Input.TimePrimitive -> "time"
  Gen.Input.TimestampPrimitive -> "timestamp"
  Gen.Input.TimestamptzPrimitive -> "timestamptz"
  Gen.Input.TimetzPrimitive -> "timetz"
  Gen.Input.TsmultirangePrimitive -> "tsmultirange"
  Gen.Input.TsrangePrimitive -> "tsrange"
  Gen.Input.TstzmultirangePrimitive -> "tstzmultirange"
  Gen.Input.TstzrangePrimitive -> "tstzrange"
  Gen.Input.UuidPrimitive -> "uuid"
  Gen.Input.XmlPrimitive -> "xml"
  Gen.Input.VarcharPrimitive -> "varchar"
  Gen.Input.BpcharPrimitive -> "bpchar"
  Gen.Input.BitPrimitive -> "bit"
  Gen.Input.VarbitPrimitive -> "varbit"
  Gen.Input.TsvectorPrimitive -> "tsvector"
  Gen.Input.TsqueryPrimitive -> "tsquery"
  Gen.Input.PointPrimitive -> "point"
  Gen.Input.LinePrimitive -> "line"
  Gen.Input.LsegPrimitive -> "lseg"
  Gen.Input.BoxPrimitive -> "box"
  Gen.Input.Box2DPrimitive -> "box2d"
  Gen.Input.Box3DPrimitive -> "box3d"
  Gen.Input.PathPrimitive -> "path"
  Gen.Input.LtreePrimitive -> "ltree"
  Gen.Input.PolygonPrimitive -> "polygon"
  Gen.Input.CirclePrimitive -> "circle"
  Gen.Input.PgSnapshotPrimitive -> "pg_snapshot"
  Gen.Input.PgLsnPrimitive -> "pg_lsn"
  Gen.Input.NamePrimitive -> "name"
  Gen.Input.HstorePrimitive -> "hstore"
  Gen.Input.CitextPrimitive -> "citext"
  Gen.Input.GeometryPrimitive -> "geometry"
  Gen.Input.GeographyPrimitive -> "geography"
  Gen.Input.OidPrimitive -> "oid"

genNameToText :: Gen.Input.Name -> Text
genNameToText name = name.inSnakeCase

-- * Serialization

-- | Serialize a signature to YAML text.
serialize :: Signature -> Text
serialize sig =
  idempotentSection <> parametersSection <> resultSection
  where
    idempotentSection =
      "idempotent: "
        <> boolToText sig.idempotent
        <> "\n"

    parametersSection =
      "parameters:"
        <> if null sig.parameters
          then " {}\n"
          else "\n" <> foldMap renderParam sig.parameters

    renderParam (name, field) =
      "  "
        <> name
        <> ":\n"
        <> renderField "    " field

    resultSection = case sig.result of
      Nothing -> ""
      Just res ->
        "result:\n"
          <> "  cardinality: "
          <> cardinalityToText res.cardinality
          <> "\n"
          <> "  columns:\n"
          <> foldMap renderColumn res.columns

    renderColumn (name, field) =
      "    "
        <> name
        <> ":\n"
        <> renderField "      " field

    renderField :: Text -> FieldSig -> Text
    renderField indent = \case
      ArrayFieldSig {typeName, notNull, elementNotNull}
        | Just (elementTypeName, dims) <- splitArrayTypeName typeName ->
            indent
              <> "type: "
              <> elementTypeName
              <> "\n"
              <> indent
              <> "not_null: "
              <> boolToText notNull
              <> "\n"
              <> indent
              <> "dims: "
              <> Text.pack (show dims)
              <> "\n"
              <> indent
              <> "element_not_null: "
              <> boolToText elementNotNull
              <> "\n"
      field ->
        indent
          <> "type: "
          <> field.typeName
          <> "\n"
          <> indent
          <> "not_null: "
          <> boolToText field.notNull
          <> "\n"

    boolToText True = "true"
    boolToText False = "false"

    cardinalityToText = \case
      CardinalityZeroOrOne -> "zero_or_one"
      CardinalityOne -> "one"
      CardinalityMany -> "many"

-- * Parsing

-- | Parse a signature from YAML text.
tryParse :: Text -> Either Text Signature
tryParse text =
  U.parseText signatureValue text
  where
    signatureValue :: U.Value Signature
    signatureValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ do
          idempotent <-
            asum
              [ U.atByKey "idempotent" (U.scalarsValue [U.boolScalar]),
                pure False
              ]
          parameters <-
            asum
              [ U.atByKey "parameters" parametersValue,
                pure []
              ]
          result <-
            asum
              [ fmap Just (U.atByKey "result" resultValue),
                pure Nothing
              ]
          pure Signature {idempotent, parameters, result}

    parametersValue :: U.Value [(Text, FieldSig)]
    parametersValue =
      U.mappingValue
        $ U.foldMapping (,) Fold.list U.textString fieldSigValue

    fieldSigValue :: U.Value FieldSig
    fieldSigValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ mkFieldSig
        <$> U.atByKey "type" typeValue
        <*> asum [Just <$> U.atByKey "dims" dimsValue, pure Nothing]
        <*> asum [U.atByKey "element_not_null" (U.scalarsValue [U.boolScalar]), pure False]
        <*> U.atByKey "not_null" (U.scalarsValue [U.boolScalar])
      where
        mkFieldSig (baseTypeName, typeDims, legacyElementNotNull) dimsOverride explicitElementNotNull notNull =
          let dims = fromMaybe typeDims dimsOverride
              elementNotNull = fromMaybe explicitElementNotNull legacyElementNotNull
              typeName = baseTypeName <> Text.replicate (fromIntegral dims) "[]"
           in if dims == 0
                then
                  ScalarFieldSig
                    { typeName,
                      notNull
                    }
                else
                  ArrayFieldSig
                    { typeName,
                      notNull,
                      elementNotNull
                    }

    typeValue :: U.Value (Text, Natural, Maybe Bool)
    typeValue =
      U.value
        [ U.stringScalar U.textString <&> \name ->
            case splitArrayTypeName name of
              Just (baseTypeName, dims) -> (baseTypeName, dims, Nothing)
              Nothing -> (name, 0, Nothing)
        ]
        ( Just
            ( U.byKeyMapping
                (U.CaseSensitive True)
                ( (\(typeName, elementNotNull) -> (typeName, 1, Just elementNotNull))
                    <$> U.atByKey "array" arrayBodyValue
                )
            )
        )
        Nothing

    dimsValue :: U.Value Natural
    dimsValue =
      U.scalarsValue [U.scientificScalar <&> scientificToDims]

    scientificToDims scientific =
      let integral = floor scientific :: Integer
       in if scientific == fromInteger integral && integral >= 0
            then fromIntegral integral
            else 0

    arrayBodyValue :: U.Value (Text, Bool)
    arrayBodyValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ U.atByKey "element" arrayElementValue

    arrayElementValue :: U.Value (Text, Bool)
    arrayElementValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ (,)
        <$> U.atByKey "name" (U.scalarsValue [U.stringScalar U.textString])
        <*> U.atByKey "not_null" (U.scalarsValue [U.boolScalar])

    resultValue :: U.Value ResultSig
    resultValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ do
          cardinality <- U.atByKey "cardinality" cardinalityValue
          columns <- U.atByKey "columns" columnsValue
          pure ResultSig {cardinality, columns}

    cardinalityValue :: U.Value Cardinality
    cardinalityValue =
      U.scalarsValue [U.stringScalar cardinalityString]

    cardinalityString =
      U.formattedString "cardinality (zero_or_one, one, many)" $ \case
        "zero_or_one" -> Right CardinalityZeroOrOne
        "one" -> Right CardinalityOne
        "many" -> Right CardinalityMany
        other -> Left ("Invalid cardinality: " <> other <> ". Expected: zero_or_one, one, many")

    columnsValue :: U.Value [(Text, FieldSig)]
    columnsValue =
      U.mappingValue
        $ U.foldMapping (,) Fold.list U.textString fieldSigValue

-- * Validation

-- | Validate a file signature against an inferred signature.
-- Returns the merged signature with user refinements applied,
-- or an error if there's an invalid mismatch.
validateAndMerge ::
  -- | Inferred signature.
  Signature ->
  -- | File signature.
  Signature ->
  Either Report.Report Signature
validateAndMerge inferred file = do
  validatedParams <- validateFields "parameters" True inferred.parameters file.parameters
  validatedResult <- case (inferred.result, file.result) of
    (Nothing, Nothing) -> Right Nothing
    (Just _, Nothing) ->
      Left
        ( mismatchError
            "result"
            "Result section missing from signature file but query returns results"
        )
    (Nothing, Just _) ->
      Left
        ( mismatchError
            "result"
            "Result section present in signature file but query returns no results"
        )
    (Just inferredResult, Just fileResult) ->
      fmap Just (validateResult inferredResult fileResult)
  Right Signature {idempotent = file.idempotent, parameters = validatedParams, result = validatedResult}

validateFields ::
  -- | Section name for error messages.
  Text ->
  -- | Whether these are parameters (vs result columns).
  Bool ->
  -- | Inferred fields.
  [(Text, FieldSig)] ->
  -- | File fields.
  [(Text, FieldSig)] ->
  Either Report.Report [(Text, FieldSig)]
validateFields section isParam inferred file = do
  let inferredNames = Set.fromList (map fst inferred)
      fileNames = Set.fromList (map fst file)
  unless (inferredNames == fileNames) do
    Left
      ( mismatchError section
          $ "Field names mismatch. Inferred: "
          <> Text.intercalate ", " (map fst inferred)
          <> ". Signature file: "
          <> Text.intercalate ", " (map fst file)
      )
  let fileMap = Map.fromList file
  for inferred \(name, inf) -> do
    let fil = fromJust (Map.lookup name fileMap)
    validated <- validateField (section <> "/" <> name) isParam inf fil
    Right (name, validated)

validateResult :: ResultSig -> ResultSig -> Either Report.Report ResultSig
validateResult inferred file = do
  validatedColumns <- validateFields "result/columns" False inferred.columns file.columns
  -- Cardinality: user can change freely, use file's value.
  Right ResultSig {cardinality = file.cardinality, columns = validatedColumns}

validateField ::
  -- | Field path for error messages.
  Text ->
  -- | Is parameter (vs result column).
  Bool ->
  -- | Inferred.
  FieldSig ->
  -- | From file.
  FieldSig ->
  Either Report.Report FieldSig
validateField fieldPath isParam inferred file = do
  unless (inferred.typeName == file.typeName) do
    Left
      ( mismatchError fieldPath
          $ "Type mismatch. Inferred: "
          <> inferred.typeName
          <> ". Signature file: "
          <> file.typeName
      )
  case (inferred.notNull, file.notNull) of
    (True, False) ->
      Left
        ( mismatchError fieldPath
            $ "not_null constraint relaxed. Inferred: true. Signature file: false. "
            <> ( if isParam
                   then "Parameters"
                   else "Result columns"
               )
            <> " cannot have their not_null constraint relaxed"
        )
    _ -> do
      let inferredElementNotNull = fieldElementNotNull inferred
          fileElementNotNull = fieldElementNotNull file <|> inferredElementNotNull
      case (inferredElementNotNull, fileElementNotNull) of
        (Just True, Just False) ->
          Left
            ( mismatchError (fieldPath <> "/element_not_null")
                $ "element_not_null constraint relaxed. Inferred: true. Signature file: false. "
                <> ( if isParam
                       then "Parameters"
                       else "Result columns"
                   )
                <> " cannot have their element_not_null constraint relaxed"
            )
        _ ->
          Right
            (mkFieldSig file.typeName file.notNull fileElementNotNull)

fieldElementNotNull :: FieldSig -> Maybe Bool
fieldElementNotNull = \case
  ArrayFieldSig {elementNotNull} -> Just elementNotNull
  ScalarFieldSig {} -> Nothing

mkFieldSig :: Text -> Bool -> Maybe Bool -> FieldSig
mkFieldSig typeName notNull = \case
  Just elementNotNull ->
    ArrayFieldSig
      { typeName,
        notNull,
        elementNotNull
      }
  Nothing ->
    ScalarFieldSig
      { typeName,
        notNull
      }

splitArrayTypeName :: Text -> Maybe (Text, Natural)
splitArrayTypeName typeName =
  let (base, dims) = go typeName 0
   in if dims > 0 && not (Text.null base) then Just (base, dims) else Nothing
  where
    go text dims =
      if Text.isSuffixOf "[]" text
        then go (Text.dropEnd 2 text) (dims + 1)
        else (text, dims)

mismatchError :: Text -> Text -> Report.Report
mismatchError fieldPath message =
  Report.Report
    { path = [],
      message = "Signature mismatch at " <> fieldPath <> ": " <> message,
      suggestion = Just "Update the signature file or fix the query",
      details = []
    }

-- * Applying refined signature to Gen.Input types

-- | Apply a validated signature to the Gen.Input types,
-- returning the refined parameters and result.
applyToQuery ::
  Signature ->
  [Gen.Input.Member] ->
  Gen.Input.Result ->
  ([Gen.Input.Member], Gen.Input.Result)
applyToQuery sig params result =
  let refinedParams = zipWith applyFieldToMember (map snd sig.parameters) params
      refinedResult =
        case (sig.result, result) of
          (Just sigResult, Gen.Input.RowsResult genResult) ->
            let updatedColumns =
                  case nonEmpty (zipWith applyFieldToMember (map snd sigResult.columns) (toList genResult.columns)) of
                    Nothing -> genResult.columns
                    Just cols -> cols
             in Gen.Input.RowsResult
                  Gen.Input.ResultRows
                    { cardinality = cardinalityToGenInput sigResult.cardinality,
                      columns = updatedColumns
                    }
          _ -> result
   in (refinedParams, refinedResult)
  where
    applyFieldToMember :: FieldSig -> Gen.Input.Member -> Gen.Input.Member
    applyFieldToMember field member =
      Gen.Input.Member
        { name = member.name,
          pgName = member.pgName,
          isNullable = not field.notNull,
          value = applyArrayElementNullability field member.value
        }

    applyArrayElementNullability :: FieldSig -> Gen.Input.Value -> Gen.Input.Value
    applyArrayElementNullability field value =
      case (fieldElementNotNull field, value.arraySettings) of
        (Just elementNotNull, Just settings) ->
          value
            { Gen.Input.arraySettings =
                Just
                  settings
                    { Gen.Input.elementIsNullable = not elementNotNull
                    }
            }
        _ -> value

spec :: Spec
spec = do
  describe "serialize and tryParse roundtrip" do
    it "roundtrips a signature with parameters and result" do
      let sig =
            Signature
              { idempotent = False,
                parameters =
                  [ ("format", ScalarFieldSig {typeName = "uuid", notNull = False}),
                    ("name", ScalarFieldSig {typeName = "text", notNull = True})
                  ],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityZeroOrOne,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "uuid", notNull = True}),
                            ("name", ScalarFieldSig {typeName = "text", notNull = True}),
                            ("released", ScalarFieldSig {typeName = "date", notNull = False})
                          ]
                      }
              }
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips a signature with empty parameters" do
      let sig =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips a signature with no result" do
      let sig =
            Signature
              { idempotent = False,
                parameters =
                  [ ("id", ScalarFieldSig {typeName = "int8", notNull = True})
                  ],
                result = Nothing
              }
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips a signature with idempotent true" do
      let sig =
            Signature
              { idempotent = True,
                parameters = [],
                result = Nothing
              }
      tryParse (serialize sig) `shouldBe` Right sig

  describe "serialize" do
    it "produces expected YAML format" do
      let sig =
            Signature
              { idempotent = False,
                parameters =
                  [ ("format", ScalarFieldSig {typeName = "uuid", notNull = False})
                  ],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityZeroOrOne,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "uuid", notNull = True})
                          ]
                      }
              }
          expected =
            "idempotent: false\n\
            \parameters:\n\
            \  format:\n\
            \    type: uuid\n\
            \    not_null: false\n\
            \result:\n\
            \  cardinality: zero_or_one\n\
            \  columns:\n\
            \    id:\n\
            \      type: uuid\n\
            \      not_null: true\n"
      serialize sig `shouldBe` expected

    it "serializes one-dimensional array types using dims syntax" do
      let sig =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ( "tracks",
                              ArrayFieldSig
                                { typeName = "track_info[]",
                                  notNull = False,
                                  elementNotNull = False
                                }
                            )
                          ]
                      }
              }
          expected =
            "idempotent: false\n\
            \parameters: {}\n\
            \result:\n\
            \  cardinality: many\n\
            \  columns:\n\
            \    tracks:\n\
            \      type: track_info\n\
            \      not_null: false\n\
            \      dims: 1\n\
            \      element_not_null: false\n"
      serialize sig `shouldBe` expected

  describe "fromInferred" do
    it "maps extension primitives to signature type names" do
      let member :: Text -> Bool -> Gen.Input.Value -> Gen.Input.Member
          member pgName isNullable value =
            Gen.Input.Member
              { name = case Name.tryFromText pgName of
                  Right name -> Name.toGenName name
                  Left err -> error ("genName: " <> show err),
                pgName,
                isNullable,
                value
              }

          primitiveValue :: Gen.Input.Primitive -> Gen.Input.Value
          primitiveValue primitive =
            Gen.Input.Value
              { arraySettings = Nothing,
                scalar = Gen.Input.PrimitiveScalar primitive
              }

          arrayValue :: Natural -> Gen.Input.Primitive -> Gen.Input.Value
          arrayValue dimensionality primitive =
            Gen.Input.Value
              { arraySettings =
                  Just
                    Gen.Input.ArraySettings
                      { dimensionality,
                        elementIsNullable = True
                      },
                scalar = Gen.Input.PrimitiveScalar primitive
              }

          sig =
            fromInferred
              [ member "ltree_path" False (primitiveValue Gen.Input.LtreePrimitive),
                member "citext_name" True (primitiveValue Gen.Input.CitextPrimitive),
                member "tags" True (primitiveValue Gen.Input.HstorePrimitive),
                member "box2d" True (primitiveValue Gen.Input.Box2DPrimitive),
                member "box3d" True (primitiveValue Gen.Input.Box3DPrimitive),
                member "geom" False (primitiveValue Gen.Input.GeometryPrimitive),
                member "geog_array" True (arrayValue 1 Gen.Input.GeographyPrimitive)
              ]
              Gen.Input.VoidResult
      sig
        `shouldBe` Signature
          { idempotent = False,
            parameters =
              [ ("ltree_path", ScalarFieldSig {typeName = "ltree", notNull = True}),
                ("citext_name", ScalarFieldSig {typeName = "citext", notNull = False}),
                ("tags", ScalarFieldSig {typeName = "hstore", notNull = False}),
                ("box2d", ScalarFieldSig {typeName = "box2d", notNull = False}),
                ("box3d", ScalarFieldSig {typeName = "box3d", notNull = False}),
                ("geom", ScalarFieldSig {typeName = "geometry", notNull = True}),
                ( "geog_array",
                  ArrayFieldSig
                    { typeName = "geography[]",
                      notNull = False,
                      elementNotNull = False
                    }
                )
              ],
            result = Nothing
          }

  describe "tryParse" do
    it "parses all cardinality values" do
      let mkSig card =
            "parameters: {}\nresult:\n  cardinality: "
              <> card
              <> "\n  columns:\n    id:\n      type: int4\n      not_null: true\n"
      fmap (.result) (tryParse (mkSig "zero_or_one"))
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityZeroOrOne,
                  columns = [("id", ScalarFieldSig {typeName = "int4", notNull = True})]
                }
          )
      fmap (.result) (tryParse (mkSig "one"))
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityOne,
                  columns = [("id", ScalarFieldSig {typeName = "int4", notNull = True})]
                }
          )
      fmap (.result) (tryParse (mkSig "many"))
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityMany,
                  columns = [("id", ScalarFieldSig {typeName = "int4", notNull = True})]
                }
          )

    it "rejects invalid cardinality" do
      let yaml =
            "parameters: {}\nresult:\n  cardinality: invalid\n  columns:\n    id:\n      type: int4\n      not_null: true\n"
      tryParse yaml `shouldSatisfy` isLeft

    it "parses explicit idempotent values" do
      fmap (.idempotent) (tryParse "idempotent: true\nparameters: {}\n")
        `shouldBe` Right True
      fmap (.idempotent) (tryParse "idempotent: false\nparameters: {}\n")
        `shouldBe` Right False

    it "defaults idempotent to false when omitted" do
      tryParse "parameters: {}\n"
        `shouldBe` Right
          Signature
            { idempotent = False,
              parameters = [],
              result = Nothing
            }

    it "parses array types with dims and element_not_null" do
      let yaml =
            "parameters: {}\n\
            \result:\n\
            \  cardinality: many\n\
            \  columns:\n\
            \    tracks:\n\
            \      type: track_info\n\
            \      not_null: false\n\
            \      dims: 1\n\
            \      element_not_null: false\n"
      fmap (.result) (tryParse yaml)
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityMany,
                  columns =
                    [ ( "tracks",
                        ArrayFieldSig
                          { typeName = "track_info[]",
                            notNull = False,
                            elementNotNull = False
                          }
                      )
                    ]
                }
          )

    it "parses legacy array syntax" do
      let yaml =
            "parameters: {}\n\
            \result:\n\
            \  cardinality: many\n\
            \  columns:\n\
            \    tracks:\n\
            \      type: track_info[]\n\
            \      not_null: false\n"
      fmap (.result) (tryParse yaml)
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityMany,
                  columns =
                    [ ( "tracks",
                        ArrayFieldSig
                          { typeName = "track_info[]",
                            notNull = False,
                            elementNotNull = False
                          }
                      )
                    ]
                }
          )

    it "defaults dims to 0 and ignores element_not_null" do
      let yaml =
            "parameters: {}\n\
            \result:\n\
            \  cardinality: many\n\
            \  columns:\n\
            \    tracks:\n\
            \      type: track_info\n\
            \      not_null: false\n\
            \      element_not_null: true\n"
      fmap (.result) (tryParse yaml)
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityMany,
                  columns =
                    [ ( "tracks",
                        ScalarFieldSig
                          { typeName = "track_info",
                            notNull = False
                          }
                      )
                    ]
                }
          )

    it "defaults element_not_null to false when dims is present" do
      let yaml =
            "parameters: {}\n\
            \result:\n\
            \  cardinality: many\n\
            \  columns:\n\
            \    tracks:\n\
            \      type: track_info\n\
            \      not_null: false\n\
            \      dims: 2\n"
      fmap (.result) (tryParse yaml)
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityMany,
                  columns =
                    [ ( "tracks",
                        ArrayFieldSig
                          { typeName = "track_info[][]",
                            notNull = False,
                            elementNotNull = False
                          }
                      )
                    ]
                }
          )

  describe "validateAndMerge" do
    it "accepts matching signatures" do
      let sig =
            Signature
              { idempotent = False,
                parameters =
                  [ ("x", ScalarFieldSig {typeName = "int4", notNull = False})
                  ],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
      validateAndMerge sig sig `shouldBe` Right sig

    it "allows parameter not_null to be made stricter (false -> true)" do
      let inferred =
            Signature
              { idempotent = False,
                parameters =
                  [ ("x", ScalarFieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
          file =
            Signature
              { idempotent = False,
                parameters =
                  [ ("x", ScalarFieldSig {typeName = "int4", notNull = True})
                  ],
                result = Nothing
              }
      validateAndMerge inferred file `shouldBe` Right file

    it "rejects parameter not_null relaxation (true -> false)" do
      let inferred =
            Signature
              { idempotent = False,
                parameters =
                  [ ("x", ScalarFieldSig {typeName = "int4", notNull = True})
                  ],
                result = Nothing
              }
          file =
            Signature
              { idempotent = False,
                parameters =
                  [ ("x", ScalarFieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "rejects result column not_null relaxation (true -> false)" do
      let inferred =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          file =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = False})
                          ]
                      }
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "allows result column not_null to be made stricter (false -> true)" do
      let inferred =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = False})
                          ]
                      }
              }
          file =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
      validateAndMerge inferred file `shouldBe` Right file

    it "allows cardinality to be changed freely" do
      let inferred =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          file =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityOne,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          expected =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityOne,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
      validateAndMerge inferred file `shouldBe` Right expected

    it "preserves idempotence from the file signature" do
      let inferred =
            Signature
              { idempotent = False,
                parameters = [],
                result = Nothing
              }
          file =
            Signature
              { idempotent = True,
                parameters = [],
                result = Nothing
              }
      validateAndMerge inferred file `shouldBe` Right file

    it "keeps inferred array element nullability when file uses legacy array syntax" do
      let inferred =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ( "tracks",
                              ArrayFieldSig
                                { typeName = "track_info[]",
                                  notNull = False,
                                  elementNotNull = True
                                }
                            )
                          ]
                      }
              }
          file =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ( "tracks",
                              ScalarFieldSig
                                { typeName = "track_info[]",
                                  notNull = False
                                }
                            )
                          ]
                      }
              }
          expected =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ( "tracks",
                              ArrayFieldSig
                                { typeName = "track_info[]",
                                  notNull = False,
                                  elementNotNull = True
                                }
                            )
                          ]
                      }
              }
      validateAndMerge inferred file `shouldBe` Right expected

    it "rejects type mismatch in parameters" do
      let inferred =
            Signature
              { idempotent = False,
                parameters =
                  [ ("x", ScalarFieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
          file =
            Signature
              { idempotent = False,
                parameters =
                  [ ("x", ScalarFieldSig {typeName = "int8", notNull = False})
                  ],
                result = Nothing
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "rejects type mismatch in result columns" do
      let inferred =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          file =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "uuid", notNull = True})
                          ]
                      }
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "rejects parameter name mismatch" do
      let inferred =
            Signature
              { idempotent = False,
                parameters =
                  [ ("x", ScalarFieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
          file =
            Signature
              { idempotent = False,
                parameters =
                  [ ("y", ScalarFieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "rejects result section mismatch (present vs absent)" do
      let inferred =
            Signature
              { idempotent = False,
                parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", ScalarFieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          file =
            Signature
              { idempotent = False,
                parameters = [],
                result = Nothing
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft
