module Logic.Features.CustomTypeSignatures.Types.CustomTypeSignatures
  ( refineFromSignatureFile,
    spec,
  )
where

import AlgebraicPath qualified as Path
import Control.Foldl qualified as Fold
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Logic.Features.Fs.Port (FsOps (..))
import Logic.Features.Naming.Types.Name qualified as Name
import Logic.Features.Reporting.Types.Report qualified as Report
import PGenieGen.Model.Input qualified as Gen.Input
import Test.Hspec
import Utils.Prelude hiding (readFile, writeFile)
import YamlUnscrambler qualified as U

-- * Types

-- | A custom-type signature as stored in a @.sig1.pgn.yaml@ file.
data CustomTypeSig
  = -- | Exact ordered list of enum variant @pgName@ values.
    EnumSig [Text]
  | -- | Ordered list of composite field entries.
    CompositeSig [(Text, CompositeFieldSig)]
  deriving stock (Eq, Show)

-- | Field encoding for composite members.  Mirrors @FieldSig@ in
-- @Logic.SignatureFile@ so array types, nullability, and element
-- nullability all remain expressible.
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
    Gen.Input.CustomTypeDefinitionEnum variants ->
      Just (EnumSig (map (.pgName) variants))
    Gen.Input.CustomTypeDefinitionComposite fields ->
      Just (CompositeSig (map memberToFieldEntry fields))
    Gen.Input.CustomTypeDefinitionDomain _ ->
      Nothing
  where
    memberToFieldEntry :: Gen.Input.Member -> (Text, CompositeFieldSig)
    memberToFieldEntry m =
      (m.pgName, compositeFieldSigFromValue m.value (not m.isNullable))

-- | Load, create, and validate the custom-type signature file for an inferred
-- custom type.
refineFromSignatureFile :: (FsOps m, MonadError Report.Report m) => Gen.Input.CustomType -> m Gen.Input.CustomType
refineFromSignatureFile ct =
  case fromInferred ct of
    Nothing -> pure ct
    Just inferredSig -> do
      let sigPath = customTypeSignatureFilePath ct.pgSchema ct.pgName
      maybeSigContent <-
        catchError
          (Just <$> readFile sigPath)
          (\(_ :: Report.Report) -> pure Nothing)
      case maybeSigContent of
        Nothing -> do
          writeFile sigPath (serialize inferredSig)
          pure ct
        Just sigContent -> do
          fileSig <- case tryParse sigContent of
            Left err ->
              throwError
                ( Report.Report
                    []
                    "Failed to parse custom-type signature file"
                    (Just "Check the YAML syntax in the signature file")
                    [("file", Path.toText sigPath), ("error", err)]
                )
            Right sig -> pure sig
          case validateAndMerge ct fileSig of
            Left err -> throwError err
            Right refined -> pure refined

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
  Gen.Input.ScalarPrimitive prim -> primitiveToTypeName prim
  Gen.Input.ScalarCustom name -> genNameToText name

primitiveToTypeName :: Gen.Input.Primitive -> Text
primitiveToTypeName = \case
  Gen.Input.PrimitiveBool -> "bool"
  Gen.Input.PrimitiveBytea -> "bytea"
  Gen.Input.PrimitiveChar -> "char"
  Gen.Input.PrimitiveCidr -> "cidr"
  Gen.Input.PrimitiveDate -> "date"
  Gen.Input.PrimitiveDatemultirange -> "datemultirange"
  Gen.Input.PrimitiveDaterange -> "daterange"
  Gen.Input.PrimitiveFloat4 -> "float4"
  Gen.Input.PrimitiveFloat8 -> "float8"
  Gen.Input.PrimitiveInet -> "inet"
  Gen.Input.PrimitiveInt2 -> "int2"
  Gen.Input.PrimitiveInt4 -> "int4"
  Gen.Input.PrimitiveInt4multirange -> "int4multirange"
  Gen.Input.PrimitiveInt4range -> "int4range"
  Gen.Input.PrimitiveInt8 -> "int8"
  Gen.Input.PrimitiveInt8multirange -> "int8multirange"
  Gen.Input.PrimitiveInt8range -> "int8range"
  Gen.Input.PrimitiveInterval -> "interval"
  Gen.Input.PrimitiveJson -> "json"
  Gen.Input.PrimitiveJsonb -> "jsonb"
  Gen.Input.PrimitiveMacaddr -> "macaddr"
  Gen.Input.PrimitiveMacaddr8 -> "macaddr8"
  Gen.Input.PrimitiveMoney -> "money"
  Gen.Input.PrimitiveNumeric -> "numeric"
  Gen.Input.PrimitiveNummultirange -> "nummultirange"
  Gen.Input.PrimitiveNumrange -> "numrange"
  Gen.Input.PrimitiveText -> "text"
  Gen.Input.PrimitiveTime -> "time"
  Gen.Input.PrimitiveTimestamp -> "timestamp"
  Gen.Input.PrimitiveTimestamptz -> "timestamptz"
  Gen.Input.PrimitiveTimetz -> "timetz"
  Gen.Input.PrimitiveTsmultirange -> "tsmultirange"
  Gen.Input.PrimitiveTsrange -> "tsrange"
  Gen.Input.PrimitiveTstzmultirange -> "tstzmultirange"
  Gen.Input.PrimitiveTstzrange -> "tstzrange"
  Gen.Input.PrimitiveUuid -> "uuid"
  Gen.Input.PrimitiveXml -> "xml"
  Gen.Input.PrimitiveVarchar -> "varchar"
  Gen.Input.PrimitiveBpchar -> "bpchar"
  Gen.Input.PrimitiveBit -> "bit"
  Gen.Input.PrimitiveVarbit -> "varbit"
  Gen.Input.PrimitiveTsvector -> "tsvector"
  Gen.Input.PrimitiveTsquery -> "tsquery"
  Gen.Input.PrimitivePoint -> "point"
  Gen.Input.PrimitiveLine -> "line"
  Gen.Input.PrimitiveLseg -> "lseg"
  Gen.Input.PrimitiveBox -> "box"
  Gen.Input.PrimitiveBox2D -> "box2d"
  Gen.Input.PrimitiveBox3D -> "box3d"
  Gen.Input.PrimitivePath -> "path"
  Gen.Input.PrimitiveLtree -> "ltree"
  Gen.Input.PrimitivePolygon -> "polygon"
  Gen.Input.PrimitiveCircle -> "circle"
  Gen.Input.PrimitivePgSnapshot -> "pg_snapshot"
  Gen.Input.PrimitivePgLsn -> "pg_lsn"
  Gen.Input.PrimitiveName -> "name"
  Gen.Input.PrimitiveHstore -> "hstore"
  Gen.Input.PrimitiveCitext -> "citext"
  Gen.Input.PrimitiveGeometry -> "geometry"
  Gen.Input.PrimitiveGeography -> "geography"
  Gen.Input.PrimitiveOid -> "oid"

genNameToText :: Gen.Input.Name -> Text
genNameToText name =
  let headText = wordToText (toList (coerce @_ @(NonEmpty _) name.head))
      tailTexts = map wordOrNumberToText name.tail
   in Text.intercalate "_" (headText : tailTexts)
  where
    wordToText :: [Gen.Input.WordChar] -> Text
    wordToText = Text.pack . map wordCharToChar

    wordCharToChar :: Gen.Input.WordChar -> Char
    wordCharToChar = \case
      Gen.Input.WordCharA -> 'a'
      Gen.Input.WordCharB -> 'b'
      Gen.Input.WordCharC -> 'c'
      Gen.Input.WordCharD -> 'd'
      Gen.Input.WordCharE -> 'e'
      Gen.Input.WordCharF -> 'f'
      Gen.Input.WordCharG -> 'g'
      Gen.Input.WordCharH -> 'h'
      Gen.Input.WordCharI -> 'i'
      Gen.Input.WordCharJ -> 'j'
      Gen.Input.WordCharK -> 'k'
      Gen.Input.WordCharL -> 'l'
      Gen.Input.WordCharM -> 'm'
      Gen.Input.WordCharN -> 'n'
      Gen.Input.WordCharO -> 'o'
      Gen.Input.WordCharP -> 'p'
      Gen.Input.WordCharQ -> 'q'
      Gen.Input.WordCharR -> 'r'
      Gen.Input.WordCharS -> 's'
      Gen.Input.WordCharT -> 't'
      Gen.Input.WordCharU -> 'u'
      Gen.Input.WordCharV -> 'v'
      Gen.Input.WordCharW -> 'w'
      Gen.Input.WordCharX -> 'x'
      Gen.Input.WordCharY -> 'y'
      Gen.Input.WordCharZ -> 'z'

    wordOrNumberToText :: Gen.Input.WordOrNumber -> Text
    wordOrNumberToText = \case
      Gen.Input.WordOrNumberWord (Gen.Input.Word w) -> wordToText (toList w)
      Gen.Input.WordOrNumberNumber n -> Text.pack (show n)

-- * Serialization

-- | Serialize a custom-type signature to YAML text.
serialize :: CustomTypeSig -> Text
serialize = \case
  EnumSig variants ->
    "enum:"
      <> if null variants
        then " []\n"
        else "\n" <> foldMap renderVariant variants
  CompositeSig fields ->
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
          [ EnumSig <$> U.atByKey "enum" variantsValue,
            CompositeSig <$> U.atByKey "composite" fieldsValue
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
    (Gen.Input.CustomTypeDefinitionEnum variants, EnumSig fileVariants) ->
      validateEnum inferred variants fileVariants
    (Gen.Input.CustomTypeDefinitionComposite fields, CompositeSig fileFields) ->
      validateComposite inferred fields fileFields
    (Gen.Input.CustomTypeDefinitionDomain _, _) ->
      Right inferred
    (Gen.Input.CustomTypeDefinitionEnum {}, CompositeSig {}) ->
      Left (mismatchError "kind" "Inferred kind is 'enum' but file signature has kind 'composite'")
    (Gen.Input.CustomTypeDefinitionComposite {}, EnumSig {}) ->
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
  Right inferred {Gen.Input.definition = Gen.Input.CustomTypeDefinitionComposite refinedFields}

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
    it "produces EnumSig from an enum custom type" do
      let ct = enumCustomType "public" "color" ["red", "green", "blue"]
      fromInferred ct `shouldBe` Just (EnumSig ["red", "green", "blue"])

    it "produces CompositeSig from a composite custom type" do
      let ct = compositeCustomType "public" "point" [("x", "float8", True), ("y", "float8", True)]
      fromInferred ct
        `shouldBe` Just
          ( CompositeSig
              [ ("x", ScalarCompositeFieldSig {typeName = "float8", notNull = False}),
                ("y", ScalarCompositeFieldSig {typeName = "float8", notNull = False})
              ]
          )

    it "returns Nothing for domain custom types" do
      let domainValue =
            Gen.Input.Value
              { arraySettings = Nothing,
                scalar = Gen.Input.ScalarPrimitive Gen.Input.PrimitiveText
              }
          ct =
            Gen.Input.CustomType
              { name = genName "my_domain",
                pgSchema = "public",
                pgName = "my_domain",
                definition = Gen.Input.CustomTypeDefinitionDomain domainValue
              }
      fromInferred ct `shouldBe` Nothing

  describe "serialize and tryParse roundtrip" do
    it "roundtrips an enum signature" do
      let sig = EnumSig ["pending", "active", "archived"]
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips an empty enum signature" do
      let sig = EnumSig []
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips a composite signature" do
      let sig =
            CompositeSig
              [ ("id", ScalarCompositeFieldSig {typeName = "uuid", notNull = True}),
                ("label", ScalarCompositeFieldSig {typeName = "text", notNull = False})
              ]
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips a composite signature with array fields" do
      let sig =
            CompositeSig
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
      let sig = CompositeSig []
      tryParse (serialize sig) `shouldBe` Right sig

  describe "serialize" do
    it "produces expected YAML for an enum" do
      let sig = EnumSig ["foo", "bar"]
          expected =
            "enum:\n\
            \  - foo\n\
            \  - bar\n"
      serialize sig `shouldBe` expected

    it "produces expected YAML for an empty enum" do
      let sig = EnumSig []
          expected =
            "enum: []\n"
      serialize sig `shouldBe` expected

    it "produces expected YAML for a composite" do
      let sig =
            CompositeSig
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
            CompositeSig
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
            fileSig = EnumSig ["red", "green", "blue"]
        validateAndMerge ct fileSig `shouldBe` Right ct

      it "rejects when a variant is added" do
        let ct = enumCustomType "public" "color" ["red", "green"]
            fileSig = EnumSig ["red", "green", "blue"]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects when a variant is removed" do
        let ct = enumCustomType "public" "color" ["red", "green", "blue"]
            fileSig = EnumSig ["red", "green"]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects when variants are reordered" do
        let ct = enumCustomType "public" "color" ["red", "green", "blue"]
            fileSig = EnumSig ["green", "red", "blue"]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects when a variant is renamed" do
        let ct = enumCustomType "public" "color" ["red", "green", "blue"]
            fileSig = EnumSig ["red", "green", "yellow"]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

    describe "composite" do
      it "succeeds when fields match exactly" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True), ("y", "float8", True)]
            fileSig =
              CompositeSig
                [ ("x", ScalarCompositeFieldSig {typeName = "float8", notNull = False}),
                  ("y", ScalarCompositeFieldSig {typeName = "float8", notNull = False})
                ]
        validateAndMerge ct fileSig `shouldSatisfy` isRight

      it "allows nullability tightening (false -> true)" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True)]
            fileSig =
              CompositeSig [("x", ScalarCompositeFieldSig {typeName = "float8", notNull = True})]
        validateAndMerge ct fileSig `shouldSatisfy` isRight

      it "rejects nullability relaxation (true -> false) when inferred is not-null" do
        let ct =
              Gen.Input.CustomType
                { name = genName "strict_type",
                  pgSchema = "public",
                  pgName = "strict_type",
                  definition =
                    Gen.Input.CustomTypeDefinitionComposite
                      [ Gen.Input.Member
                          { name = genName "val",
                            pgName = "val",
                            isNullable = False,
                            value = Gen.Input.Value {arraySettings = Nothing, scalar = Gen.Input.ScalarPrimitive Gen.Input.PrimitiveText}
                          }
                      ]
                }
            fileSig =
              CompositeSig [("val", ScalarCompositeFieldSig {typeName = "text", notNull = False})]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects type mismatch on a field" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True)]
            fileSig =
              CompositeSig [("x", ScalarCompositeFieldSig {typeName = "int4", notNull = False})]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects mismatched field-name sets" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True), ("y", "float8", True)]
            fileSig =
              CompositeSig
                [ ("x", ScalarCompositeFieldSig {typeName = "float8", notNull = False}),
                  ("z", ScalarCompositeFieldSig {typeName = "float8", notNull = False})
                ]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "rejects a kind mismatch (enum inferred vs composite file)" do
        let ct = enumCustomType "public" "color" ["red", "green"]
            fileSig =
              CompositeSig
                [ ("red", ScalarCompositeFieldSig {typeName = "text", notNull = False})
                ]
        validateAndMerge ct fileSig `shouldSatisfy` isLeft

      it "returns refined composite with file nullability applied" do
        let ct = compositeCustomType "public" "point" [("x", "float8", True), ("y", "float8", True)]
            fileSig =
              CompositeSig
                [ ("x", ScalarCompositeFieldSig {typeName = "float8", notNull = True}),
                  ("y", ScalarCompositeFieldSig {typeName = "float8", notNull = False})
                ]
            result = validateAndMerge ct fileSig
        case result of
          Left err -> expectationFailure ("Expected Right but got Left: " <> show err)
          Right refined ->
            case refined.definition of
              Gen.Input.CustomTypeDefinitionComposite fields ->
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
        Gen.Input.CustomTypeDefinitionEnum
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
        Gen.Input.CustomTypeDefinitionComposite
          ( map
              ( \(fieldPgName, typeName, _notNull) ->
                  Gen.Input.Member
                    { name = genName fieldPgName,
                      pgName = fieldPgName,
                      isNullable = True,
                      value =
                        Gen.Input.Value
                          { arraySettings = Nothing,
                            scalar = Gen.Input.ScalarPrimitive (textToPrimitive typeName)
                          }
                    }
              )
              fieldSpecs
          )
    }

textToPrimitive :: Text -> Gen.Input.Primitive
textToPrimitive = \case
  "float8" -> Gen.Input.PrimitiveFloat8
  "text" -> Gen.Input.PrimitiveText
  "uuid" -> Gen.Input.PrimitiveUuid
  "int4" -> Gen.Input.PrimitiveInt4
  other -> error ("textToPrimitive: unknown primitive " <> show other)
