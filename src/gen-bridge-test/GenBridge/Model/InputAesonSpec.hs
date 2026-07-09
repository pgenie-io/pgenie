module GenBridge.Model.InputAesonSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import GenBridge.Fixtures.Project1 qualified as Fixtures.Project1
import GenBridge.Model.Input qualified as Input
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "Input model JSON contract" do
    it "encodes the Project1 fixture using the pinned kebab-case, single-field-object contract" do
      Aeson.toJSON Fixtures.Project1.input `shouldBe` project1Json

    it "decodes the pinned JSON back into the Project1 fixture value" do
      case Aeson.fromJSON project1Json of
        Aeson.Error err -> expectationFailure err
        Aeson.Success decoded -> decoded `shouldBe` Fixtures.Project1.input

    it "encodes Name fields as literal camelCase keys, not kebab-case" do
      Aeson.toJSON exampleName
        `shouldBe` [aesonQQ|
          {
            "inCamelCase": "c",
            "inPascalCase": "p",
            "inKebabCase": "k",
            "inTrainCase": "t",
            "inScreamingKebabCase": "sk",
            "inSnakeCase": "s",
            "inCamelSnakeCase": "cs",
            "inScreamingSnakeCase": "ss"
          }
        |]

    it "encodes an all-nullary sum (Primitive) as a bare kebab-case string, splitting before digits" do
      Aeson.toJSON Input.Int4Primitive `shouldBe` Aeson.String "int-4"

    it "encodes a non-nullary sum alternative (Scalar) as a single-field object keyed by the stripped, kebab-cased constructor" do
      Aeson.toJSON (Input.PrimitiveScalar Input.Int4Primitive)
        `shouldBe` [aesonQQ| { "primitive": "int-4" } |]

      Aeson.toJSON (Input.CustomScalar exampleName)
        `shouldBe` [aesonQQ|
          {
            "custom": {
              "inCamelCase": "c",
              "inPascalCase": "p",
              "inKebabCase": "k",
              "inTrainCase": "t",
              "inScreamingKebabCase": "sk",
              "inSnakeCase": "s",
              "inCamelSnakeCase": "cs",
              "inScreamingSnakeCase": "ss"
            }
          }
        |]

    it "encodes nullary alternatives of a mixed sum (Result) as a single-field object with empty-array contents" do
      Aeson.toJSON Input.VoidResult `shouldBe` [aesonQQ| { "void": [] } |]
      Aeson.toJSON Input.RowsAffectedResult `shouldBe` [aesonQQ| { "rows-affected": [] } |]

    it "kebab-cases record field names" do
      Aeson.toJSON
        Input.Var
          { name = exampleName,
            rawName = "raw",
            paramIndex = 1
          }
        `shouldBe` [aesonQQ|
          {
            "name": {
              "inCamelCase": "c",
              "inPascalCase": "p",
              "inKebabCase": "k",
              "inTrainCase": "t",
              "inScreamingKebabCase": "sk",
              "inSnakeCase": "s",
              "inCamelSnakeCase": "cs",
              "inScreamingSnakeCase": "ss"
            },
            "raw-name": "raw",
            "param-index": 1
          }
        |]

    it "encodes a composite custom type definition tagged by stripping the type name" do
      Aeson.toJSON (Input.CompositeCustomTypeDefinition [])
        `shouldBe` [aesonQQ| { "composite": [] } |]

    it "encodes an enum custom type definition tagged by stripping the type name, kebab-casing enum variant fields" do
      Aeson.toJSON (Input.EnumCustomTypeDefinition [Input.EnumVariant {name = exampleName, pgName = "raw_enum"}])
        `shouldBe` [aesonQQ|
          {
            "enum": [
              {
                "name": {
                  "inCamelCase": "c", "inPascalCase": "p", "inKebabCase": "k", "inTrainCase": "t",
                  "inScreamingKebabCase": "sk", "inSnakeCase": "s", "inCamelSnakeCase": "cs", "inScreamingSnakeCase": "ss"
                },
                "pg-name": "raw_enum"
              }
            ]
          }
        |]

    it "encodes a domain custom type definition tagged by stripping the type name" do
      Aeson.toJSON
        ( Input.DomainCustomTypeDefinition
            Input.Value {arraySettings = Nothing, scalar = Input.PrimitiveScalar Input.Int4Primitive}
        )
        `shouldBe` [aesonQQ| { "domain": { "array-settings": null, "scalar": { "primitive": "int-4" } } } |]

    it "encodes a CustomType record with kebab-cased pg-schema/pg-name fields" do
      Aeson.toJSON
        Input.CustomType
          { name = exampleName,
            pgSchema = "public",
            pgName = "status",
            definition = Input.EnumCustomTypeDefinition []
          }
        `shouldBe` [aesonQQ|
          {
            "name": {
              "inCamelCase": "c", "inPascalCase": "p", "inKebabCase": "k", "inTrainCase": "t",
              "inScreamingKebabCase": "sk", "inSnakeCase": "s", "inCamelSnakeCase": "cs", "inScreamingSnakeCase": "ss"
            },
            "pg-schema": "public",
            "pg-name": "status",
            "definition": { "enum": [] }
          }
        |]

    it "encodes non-null ArraySettings with kebab-cased dimensionality/element-is-nullable fields" do
      Aeson.toJSON
        Input.Value
          { arraySettings = Just Input.ArraySettings {dimensionality = 2, elementIsNullable = True},
            scalar = Input.PrimitiveScalar Input.Int4Primitive
          }
        `shouldBe` [aesonQQ|
          {
            "array-settings": {"dimensionality": 2, "element-is-nullable": true},
            "scalar": {"primitive": "int-4"}
          }
        |]

exampleName :: Input.Name
exampleName =
  Input.Name
    { inCamelCase = "c",
      inPascalCase = "p",
      inKebabCase = "k",
      inTrainCase = "t",
      inScreamingKebabCase = "sk",
      inSnakeCase = "s",
      inCamelSnakeCase = "cs",
      inScreamingSnakeCase = "ss"
    }

-- | Captured by actually running gen-sdk's pre-drop AesonDeriver-based
-- encoder (commit da426a3^) against this exact fixture value, then
-- adjusting the one field whose *type* has since legitimately changed:
-- 'Input.Query.srcPath' is now 'Text' (was 'Path'), so it no longer gets
-- the old Path type's "./" prefix normalization.
project1Json :: Aeson.Value
project1Json =
  [aesonQQ|
    {
      "space": {
        "inCamelCase": "demo", "inCamelSnakeCase": "Demo", "inKebabCase": "demo",
        "inPascalCase": "Demo", "inScreamingKebabCase": "DEMO", "inScreamingSnakeCase": "DEMO",
        "inSnakeCase": "demo", "inTrainCase": "Demo"
      },
      "name": {
        "inCamelCase": "demoProject", "inCamelSnakeCase": "Demo_Project", "inKebabCase": "demo-project",
        "inPascalCase": "DemoProject", "inScreamingKebabCase": "DEMO-PROJECT", "inScreamingSnakeCase": "DEMO_PROJECT",
        "inSnakeCase": "demo_project", "inTrainCase": "Demo-Project"
      },
      "version": { "major": 1, "minor": 0, "patch": 0 },
      "custom-types": [],
      "queries": [
        {
          "name": {
            "inCamelCase": "getUser", "inCamelSnakeCase": "Get_User", "inKebabCase": "get-user",
            "inPascalCase": "GetUser", "inScreamingKebabCase": "GET-USER", "inScreamingSnakeCase": "GET_USER",
            "inSnakeCase": "get_user", "inTrainCase": "Get-User"
          },
          "src-path": "queries/get_user.sql",
          "identity": false,
          "idempotent": true,
          "params": [
            {
              "name": {
                "inCamelCase": "userId", "inCamelSnakeCase": "User_Id", "inKebabCase": "user-id",
                "inPascalCase": "UserId", "inScreamingKebabCase": "USER-ID", "inScreamingSnakeCase": "USER_ID",
                "inSnakeCase": "user_id", "inTrainCase": "User-Id"
              },
              "pg-name": "user_id",
              "is-nullable": false,
              "value": { "array-settings": null, "scalar": { "primitive": "int-4" } }
            }
          ],
          "result": {
            "rows": {
              "cardinality": "optional",
              "columns": [
                {
                  "name": {
                    "inCamelCase": "id", "inCamelSnakeCase": "Id", "inKebabCase": "id",
                    "inPascalCase": "Id", "inScreamingKebabCase": "ID", "inScreamingSnakeCase": "ID",
                    "inSnakeCase": "id", "inTrainCase": "Id"
                  },
                  "pg-name": "id",
                  "is-nullable": false,
                  "value": { "array-settings": null, "scalar": { "primitive": "int-4" } }
                },
                {
                  "name": {
                    "inCamelCase": "name", "inCamelSnakeCase": "Name", "inKebabCase": "name",
                    "inPascalCase": "Name", "inScreamingKebabCase": "NAME", "inScreamingSnakeCase": "NAME",
                    "inSnakeCase": "name", "inTrainCase": "Name"
                  },
                  "pg-name": "name",
                  "is-nullable": false,
                  "value": { "array-settings": null, "scalar": { "primitive": "text" } }
                },
                {
                  "name": {
                    "inCamelCase": "email", "inCamelSnakeCase": "Email", "inKebabCase": "email",
                    "inPascalCase": "Email", "inScreamingKebabCase": "EMAIL", "inScreamingSnakeCase": "EMAIL",
                    "inSnakeCase": "email", "inTrainCase": "Email"
                  },
                  "pg-name": "email",
                  "is-nullable": true,
                  "value": { "array-settings": null, "scalar": { "primitive": "text" } }
                }
              ]
            }
          },
          "fragments": [
            { "sql": "SELECT id, name, email FROM users WHERE id = " },
            {
              "var": {
                "name": {
                  "inCamelCase": "userId", "inCamelSnakeCase": "User_Id", "inKebabCase": "user-id",
                  "inPascalCase": "UserId", "inScreamingKebabCase": "USER-ID", "inScreamingSnakeCase": "USER_ID",
                  "inSnakeCase": "user_id", "inTrainCase": "User-Id"
                },
                "raw-name": "user_id",
                "param-index": 1
              }
            }
          ]
        }
      ],
      "migrations": [
        {
          "name": "001_create_users",
          "sql": "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT);"
        }
      ]
    }
  |]
