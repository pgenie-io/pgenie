-- |
-- Custom-type signature files (@.sig1.pgn.yaml@): serialization, parsing,
-- and validation-and-merge against an inferred custom type.
module Logic.Domain.CustomTypeSignature
  ( CustomTypeSig (..),
    CompositeFieldSig (..),
    customTypeSignatureFilePath,
    fromInferred,
    serialize,
    tryParse,
    validateAndMerge,
    spec,
  )
where

import AlgebraicPath qualified as Path
import Control.Foldl qualified as Fold
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GenBridge.Model.Input qualified as Gen.Input
import Logic.Domain.Name qualified as Name
import Logic.Domain.Report qualified as Report
import Test.Hspec
import Utils.Prelude
import YamlUnscrambler qualified as U

-- * Types

-- | A custom-type signature as stored in a @.sig1.pgn.yaml@ file.
data CustomTypeSig
  = -- | Exact ordered list of enum variant @pgName@ values.
    EnumCustomTypeSig [Text]
  | -- | Ordered list of composite field entries.
    CompositeCustomTypeSig [(Text, CompositeFieldSig)]
  deriving stock (Eq, Show)

-- | Field encoding for composite members.  Mirrors 'Logic.Domain.QuerySignature.FieldSig'
-- so array types, nullability, and element nullability all remain expressible.
data CompositeFieldSig
  = ScalarCompositeFieldSig
      { typeName :: Text,
        notNull :: Bool
      }
  | ArrayCompositeFieldSig
      { typeName :: Text,
        notNull :: Bool,
        elementNotNull :: Bool
      }
  deriving stock (Eq, Show)

-- * Path computation

-- | Compute the signature file path for a custom type given its PostgreSQL
-- schema and name.
--
-- >>> customTypeSignatureFilePath "public" "my_status"
-- "types/public/my_status.sig1.pgn.yaml"
customTypeSignatureFilePath :: Text -> Text -> Path
customTypeSignatureFilePath schema name =
  "types"
    <> fromString (Text.unpack schema)
    <> Path.addExtension "sig1.pgn.yaml" (fromString (Text.unpack name))

-- * Conversion from inferred types

-- | Create a custom-type signature from an inferred @Gen.Input.CustomType@.
-- Returns @Nothing@ for domain types, which have no signature file.
fromInferred :: Gen.Input.CustomType -> Maybe CustomTypeSig
fromInferred ct =
  case ct.definition of
    Gen.Input.EnumCustomTypeDefinition variants ->
      Just (EnumCustomTypeSig (map (.pgName) variants))
    Gen.Input.CompositeCustomTypeDefinition fields ->
      Just (CompositeCustomTypeSig (map memberToFieldEntry fields))
    Gen.Input.DomainCustomTypeDefinition _ ->
      Nothing
  where
    memberToFieldEntry :: Gen.Input.Member -> (Text, CompositeFieldSig)
    memberToFieldEntry m =
      (m.pgName, compositeFieldSigFromValue m.value (not m.isNullable))

compositeFieldSigFromValue :: Gen.Input.Value -> Bool -> CompositeFieldSig
compositeFieldSigFromValue value fieldNotNull =
  case value.arraySettings of
    Just settings ->
      ArrayCompositeFieldSig
        { typeName = valueToTypeName value,
          notNull = fieldNotNull,
          elementNotNull = not settings.elementIsNullable
        }
    Nothing ->
      ScalarCompositeFieldSig
        { typeName = valueToTypeName value,
          notNull = fieldNotNull
        }

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

-- | Serialize a custom-type signature to YAML text.
serialize :: CustomTypeSig -> Text
serialize = \case
  EnumCustomTypeSig variants ->
    "enum:"
      <> if null variants
        then " []\n"
        else "\n" <> foldMap renderVariant variants
  CompositeCustomTypeSig fields ->
    "composite:"
      <> if null fields
        then " {}\n"
        else "\n" <> foldMap renderField fields
  where
    renderVariant name =
      "  - " <> name <> "\n"

    renderField (name, field) =
      "  " <> name <> ":\n" <> renderCompositeFieldSig "    " field

    renderCompositeFieldSig :: Text -> CompositeFieldSig -> Text
    renderCompositeFieldSig indent = \case
      ArrayCompositeFieldSig {typeName, notNull, elementNotNull}
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

-- * Parsing

-- | Parse a custom-type signature from YAML text.
tryParse :: Text -> Either Text CustomTypeSig
tryParse text =
  U.parseText customTypeSigValue text
  where
    customTypeSigValue :: U.Value CustomTypeSig
    customTypeSigValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ asum
          [ EnumCustomTypeSig <$> U.atByKey "enum" variantsValue,
            CompositeCustomTypeSig <$> U.atByKey "composite" fieldsValue
          ]

    variantsValue :: U.Value [Text]
    variantsValue =
      U.sequenceValue
        $ U.foldSequence Fold.list (U.scalarsValue [U.stringScalar U.textString])

    fieldsValue :: U.Value [(Text, CompositeFieldSig)]
    fieldsValue =
      U.mappingValue
        $ U.foldMapping (,) Fold.list U.textString compositeFieldSigValue

    compositeFieldSigValue :: U.Value CompositeFieldSig
    compositeFieldSigValue =
      U.mappingValue
        $ U.byKeyMapping (U.CaseSensitive True)
        $ mkCompositeFieldSig
        <$> U.atByKey "type" typeValue
        <*> asum [Just <$> U.atByKey "dims" dimsValue, pure Nothing]
        <*> asum [U.atByKey "element_not_null" (U.scalarsValue [U.boolScalar]), pure False]
        <*> U.atByKey "not_null" (U.scalarsValue [U.boolScalar])
      where
        mkCompositeFieldSig (baseTypeName, typeDims, legacyElementNotNull) dimsOverride explicitElementNotNull notNull =
          let dims = fromMaybe typeDims dimsOverride
              elementNotNull = fromMaybe explicitElementNotNull legacyElementNotNull
              typeName = baseTypeName <> Text.replicate (fromIntegral dims) "[]"
           in if dims == 0
                then
                  ScalarCompositeFieldSig
                    { typeName,
                      notNull
                    }
                else
                  ArrayCompositeFieldSig
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

-- * Validation and merging

-- | Validate a file custom-type signature against the inferred type and return
-- the refined @Gen.Input.CustomType@, or an error describing the mismatch.
validateAndMerge ::
  -- | Inferred custom type.
  Gen.Input.CustomType ->
  -- | Signature from file.
  CustomTypeSig ->
  Either Report.Report Gen.Input.CustomType
validateAndMerge inferred fileSig =
  case (inferred.definition, fileSig) of
    (Gen.Input.EnumCustomTypeDefinition variants, EnumCustomTypeSig fileVariants) ->
      validateEnum inferred variants fileVariants
    (Gen.Input.CompositeCustomTypeDefinition fields, CompositeCustomTypeSig fileFields) ->
      validateComposite inferred fields fileFields
    (Gen.Input.DomainCustomTypeDefinition _, _) ->
      Right inferred
    (Gen.Input.EnumCustomTypeDefinition {}, CompositeCustomTypeSig {}) ->
      Left (mismatchError "kind" "Inferred kind is 'enum' but file signature has kind 'composite'")
    (Gen.Input.CompositeCustomTypeDefinition {}, EnumCustomTypeSig {}) ->
      Left (mismatchError "kind" "Inferred kind is 'composite' but file signature has kind 'enum'")

validateEnum ::
  Gen.Input.CustomType ->
  [Gen.Input.EnumVariant] ->
  [Text] ->
  Either Report.Report Gen.Input.CustomType
validateEnum inferred variants fileVariants = do
  let inferredNames = map (.pgName) variants
  unless (inferredNames == fileVariants) do
    Left
      ( mismatchError "variants"
          $ "Enum variants mismatch. Inferred (ordered): "
          <> Text.intercalate ", " inferredNames
          <> ". Signature file (ordered): "
          <> Text.intercalate ", " fileVariants
      )
  Right inferred

validateComposite ::
  Gen.Input.CustomType ->
  [Gen.Input.Member] ->
  [(Text, CompositeFieldSig)] ->
  Either Report.Report Gen.Input.CustomType
validateComposite inferred inferredFields fileFields = do
  let inferredNames = Set.fromList (map (.pgName) inferredFields)
      fileNames = Set.fromList (map fst fileFields)
  unless (inferredNames == fileNames) do
    Left
      ( mismatchError "fields"
          $ "Composite field names mismatch. Inferred: "
          <> Text.intercalate ", " (map (.pgName) inferredFields)
          <> ". Signature file: "
          <> Text.intercalate ", " (map fst fileFields)
      )
  let fileMap = Map.fromList fileFields
  refinedFields <- for inferredFields \inferredMember -> do
    let fileSigField = fromJust (Map.lookup inferredMember.pgName fileMap)
        inferredSigField = compositeFieldSigFromValue inferredMember.value (not inferredMember.isNullable)
    validatedSigField <-
      validateCompositeField
        ("fields/" <> inferredMember.pgName)
        inferredSigField
        fileSigField
    pure (applyCompositeFieldSigToMember validatedSigField inferredMember)
  Right inferred {Gen.Input.definition = Gen.Input.CompositeCustomTypeDefinition refinedFields}

validateCompositeField ::
  -- | Field path for error messages.
  Text ->
  -- | Inferred.
  CompositeFieldSig ->
  -- | From file.
  CompositeFieldSig ->
  Either Report.Report CompositeFieldSig
validateCompositeField fieldPath inferred file = do
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
            <> "Composite fields cannot have their not_null constraint relaxed"
        )
    _ -> do
      let inferredElementNotNull = compositeFieldElementNotNull inferred
          fileElementNotNull = compositeFieldElementNotNull file <|> inferredElementNotNull
      case (inferredElementNotNull, fileElementNotNull) of
        (Just True, Just False) ->
          Left
            ( mismatchError (fieldPath <> "/element_not_null")
                $ "element_not_null constraint relaxed. Inferred: true. Signature file: false. "
                <> "Composite fields cannot have their element_not_null constraint relaxed"
            )
        _ ->
          Right
            (mkCompositeFieldSig file.typeName file.notNull fileElementNotNull)

compositeFieldElementNotNull :: CompositeFieldSig -> Maybe Bool
compositeFieldElementNotNull = \case
  ArrayCompositeFieldSig {elementNotNull} -> Just elementNotNull
  ScalarCompositeFieldSig {} -> Nothing

mkCompositeFieldSig :: Text -> Bool -> Maybe Bool -> CompositeFieldSig
mkCompositeFieldSig typeName notNull = \case
  Just elementNotNull ->
    ArrayCompositeFieldSig
      { typeName,
        notNull,
        elementNotNull
      }
  Nothing ->
    ScalarCompositeFieldSig
      { typeName,
        notNull
      }

applyCompositeFieldSigToMember :: CompositeFieldSig -> Gen.Input.Member -> Gen.Input.Member
applyCompositeFieldSigToMember field member =
  Gen.Input.Member
    { name = member.name,
      pgName = member.pgName,
      isNullable = not field.notNull,
      value = applyArrayElementNullability field member.value
    }

applyArrayElementNullability :: CompositeFieldSig -> Gen.Input.Value -> Gen.Input.Value
applyArrayElementNullability field value =
  case (compositeFieldElementNotNull field, value.arraySettings) of
    (Just elementNotNull, Just settings) ->
      value
        { Gen.Input.arraySettings =
            Just
              settings
                { Gen.Input.elementIsNullable = not elementNotNull
                }
        }
    _ -> value

-- * Helpers

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
      message = "Custom-type signature mismatch at " <> fieldPath <> ": " <> message,
      suggestion = Just "Update the signature file or fix the custom type",
      details = []
    }

spec :: Spec
spec = do
  describe "customTypeSignatureFilePath" do
    it "maps schema and name to types/<schema>/<name>.sig1.pgn.yaml" do
      customTypeSignatureFilePath "public" "my_status"
        `shouldBe` ("types" <> "public" <> Path.addExtension "sig1.pgn.yaml" "my_status")

    it "maps a different schema correctly" do
      Path.toText (customTypeSignatureFilePath "audit" "event_kind")
        `shouldBe` "./types/audit/event_kind.sig1.pgn.yaml"

  describe "fromInferred" do
    it "produces EnumCustomTypeSig from an enum custom type" do
      let ct = enumCustomType "public" "color" ["red", "green", "blue"]
      fromInferred ct `shouldBe` Just (EnumCustomTypeSig ["red", "green", "blue"])

    it "produces CompositeCustomTypeSig from a composite custom type" do
      let ct = compositeCustomType "public" "point" [("x", "float8", True), ("y", "float8", True)]
      fromInferred ct
        `shouldBe` Just
          ( CompositeCustomTypeSig
              [ ("x", ScalarCompositeFieldSig {typeName = "float8", notNull = False}),
                ("y", ScalarCompositeFieldSig {typeName = "float8", notNull = False})
              ]
          )

    it "returns Nothing for domain custom types" do
      let domainValue =
            Gen.Input.Value
              { arraySettings = Nothing,
                scalar = Gen.Input.PrimitiveScalar Gen.Input.TextPrimitive
              }
          ct =
            Gen.Input.CustomType
              { name = genName "my_domain",
                pgSchema = "public",
                pgName = "my_domain",
                definition = Gen.Input.DomainCustomTypeDefinition domainValue
              }
      fromInferred ct `shouldBe` Nothing

  describe "serialize and tryParse roundtrip" do
    it "roundtrips an enum signature" do
      let sig = EnumCustomTypeSig ["pending", "active", "archived"]
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips an empty enum signature" do
      let sig = EnumCustomTypeSig []
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips a composite signature" do
      let sig =
            CompositeCustomTypeSig
              [ ("id", ScalarCompositeFieldSig {typeName = "uuid", notNull = True}),
                ("label", ScalarCompositeFieldSig {typeName = "text", notNull = False})
              ]
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips a composite signature with array fields" do
      let sig =
            CompositeCustomTypeSig
              [ ( "tags",
                  ArrayCompositeFieldSig
                    { typeName = "text[]",
                      notNull = False,
                      elementNotNull = True
                    }
                )
              ]
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips an empty composite signature" do
      let sig = CompositeCustomTypeSig []
      tryParse (serialize sig) `shouldBe` Right sig

  describe "serialize" do
    it "produces expected YAML for an enum" do
      let sig = EnumCustomTypeSig ["foo", "bar"]
          expected =
            "enum:\n\
            \  - foo\n\
            \  - bar\n"
      serialize sig `shouldBe` expected

    it "produces expected YAML for an empty enum" do
      let sig = EnumCustomTypeSig []
          expected =
            "enum: []\n"
      serialize sig `shouldBe` expected

    it "produces expected YAML for a composite" do
      let sig =
            CompositeCustomTypeSig
              [ ("id", ScalarCompositeFieldSig {typeName = "uuid", notNull = True}),
                ("notes", ScalarCompositeFieldSig {typeName = "text", notNull = False})
              ]
          expected =
            "composite:\n\
            \  id:\n\
            \    type: uuid\n\
            \    not_null: true\n\
            \  notes:\n\
            \    type: text\n\
            \    not_null: false\n"
      serialize sig `shouldBe` expected

    it "serializes array composite fields using dims syntax" do
      let sig =
            CompositeCustomTypeSig
              [ ( "items",
                  ArrayCompositeFieldSig
                    { typeName = "int4[]",
                      notNull = False,
                      elementNotNull = True
                    }
                )
              ]
          expected =
            "composite:\n\
            \  items:\n\
            \    type: int4\n\
            \    not_null: false\n\
            \    dims: 1\n\
            \    element_not_null: true\n"
      serialize sig `shouldBe` expected

  describe "validateAndMerge" do
    describe "enum" do
      it "succeeds when variants match exactly" do
        let ct = enumCustomType "public" "color" ["red", "green", "blue"]
            fileSig = EnumCustomTypeSig ["red", "green", "blue"]
        validateAndMerge ct fileSig `shouldBe` Right ct

      it "rejects when a variant is added" do
        let ct = enumCustomType "public" "color" ["red", "green"]
            fileSig = EnumCustomTypeSig ["red", "green", "blue"]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects when a variant is removed" do
        let ct = enumCustomType "public" "color" ["red", "green", "blue"]
            fileSig = EnumCustomTypeSig ["red", "green"]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects when variants are reordered" do
        let ct = enumCustomType "public" "color" ["red", "green", "blue"]
            fileSig = EnumCustomTypeSig ["green", "red", "blue"]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects when a variant is renamed" do
        let ct = enumCustomType "public" "color" ["red", "green", "blue"]
            fileSig = EnumCustomTypeSig ["red", "green", "yellow"]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

    describe "composite" do
      it "succeeds when fields match exactly" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True), ("y", "float8", True)]
            fileSig =
              CompositeCustomTypeSig
                [ ("x", ScalarCompositeFieldSig {typeName = "float8", notNull = False}),
                  ("y", ScalarCompositeFieldSig {typeName = "float8", notNull = False})
                ]
        validateAndMerge ct fileSig `shouldSatisfy` isRight

      it "allows nullability tightening (false -> true)" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True)]
            fileSig =
              CompositeCustomTypeSig [("x", ScalarCompositeFieldSig {typeName = "float8", notNull = True})]
        validateAndMerge ct fileSig `shouldSatisfy` isRight

      it "rejects nullability relaxation (true -> false) when inferred is not-null" do
        let ct =
              Gen.Input.CustomType
                { name = genName "strict_type",
                  pgSchema = "public",
                  pgName = "strict_type",
                  definition =
                    Gen.Input.CompositeCustomTypeDefinition
                      [ Gen.Input.Member
                          { name = genName "val",
                            pgName = "val",
                            isNullable = False,
                            value = Gen.Input.Value {arraySettings = Nothing, scalar = Gen.Input.PrimitiveScalar Gen.Input.TextPrimitive}
                          }
                      ]
                }
            fileSig =
              CompositeCustomTypeSig [("val", ScalarCompositeFieldSig {typeName = "text", notNull = False})]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects type mismatch on a field" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True)]
            fileSig =
              CompositeCustomTypeSig [("x", ScalarCompositeFieldSig {typeName = "int4", notNull = False})]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects mismatched field-name sets" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True), ("y", "float8", True)]
            fileSig =
              CompositeCustomTypeSig
                [ ("x", ScalarCompositeFieldSig {typeName = "float8", notNull = False}),
                  ("z", ScalarCompositeFieldSig {typeName = "float8", notNull = False})
                ]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects a kind mismatch (enum inferred vs composite file)" do
        let ct = enumCustomType "public" "color" ["red", "green"]
            fileSig =
              CompositeCustomTypeSig
                [ ("red", ScalarCompositeFieldSig {typeName = "text", notNull = False})
                ]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "returns refined composite with file nullability applied" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True), ("y", "float8", True)]
            fileSig =
              CompositeCustomTypeSig
                [ ("x", ScalarCompositeFieldSig {typeName = "float8", notNull = True}),
                  ("y", ScalarCompositeFieldSig {typeName = "float8", notNull = False})
                ]
            result = validateAndMerge ct fileSig
        case result of
          Left err -> expectationFailure ("Expected Right but got Left: " <> show err)
          Right refined ->
            case refined.definition of
              Gen.Input.CompositeCustomTypeDefinition fields ->
                case fields of
                  [xField, yField] -> do
                    xField.isNullable `shouldBe` False
                    yField.isNullable `shouldBe` True
                  _ ->
                    expectationFailure ("Expected exactly 2 fields, got " <> show (length fields))
              _ ->
                expectationFailure "Expected composite definition"

genName :: Text -> Gen.Input.Name
genName text =
  case Name.tryFromText text of
    Right name -> Name.toGenName name
    Left err -> error ("genName: " <> show err)

enumCustomType :: Text -> Text -> [Text] -> Gen.Input.CustomType
enumCustomType schema pgName pgNames =
  Gen.Input.CustomType
    { name = genName pgName,
      pgSchema = schema,
      pgName,
      definition =
        Gen.Input.EnumCustomTypeDefinition
          ( map
              ( \v ->
                  Gen.Input.EnumVariant
                    { name = genName v,
                      pgName = v
                    }
              )
              pgNames
          )
    }

compositeCustomType :: Text -> Text -> [(Text, Text, Bool)] -> Gen.Input.CustomType
compositeCustomType schema pgName fieldSpecs =
  Gen.Input.CustomType
    { name = genName pgName,
      pgSchema = schema,
      pgName,
      definition =
        Gen.Input.CompositeCustomTypeDefinition
          ( map
              ( \(fieldPgName, typeName, _notNull) ->
                  Gen.Input.Member
                    { name = genName fieldPgName,
                      pgName = fieldPgName,
                      isNullable = True,
                      value =
                        Gen.Input.Value
                          { arraySettings = Nothing,
                            scalar = Gen.Input.PrimitiveScalar (textToPrimitive typeName)
                          }
                    }
              )
              fieldSpecs
          )
    }

textToPrimitive :: Text -> Gen.Input.Primitive
textToPrimitive = \case
  "float8" -> Gen.Input.Float8Primitive
  "text" -> Gen.Input.TextPrimitive
  "uuid" -> Gen.Input.UuidPrimitive
  "int4" -> Gen.Input.Int4Primitive
  other -> error ("textToPrimitive: unknown primitive " <> show other)
