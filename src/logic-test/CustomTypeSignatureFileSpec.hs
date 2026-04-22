module CustomTypeSignatureFileSpec (spec) where

import AlgebraicPath qualified as Path
import Logic.CustomTypeSignatureFile
import Logic.Name qualified as Name
import PGenieGen.Model.Input qualified as Gen.Input
import Test.Hspec
import Utils.Prelude

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

-- * Helpers

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
