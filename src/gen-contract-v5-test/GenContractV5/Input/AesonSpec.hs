{-# LANGUAGE QuasiQuotes #-}

module GenContractV5.Input.AesonSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import GenContractV5.Fixtures.Project1 qualified as Fixtures.Project1
import GenContractV5.Input qualified as Input
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

    it "encodes Name fields as kebab-case keys" do
      Aeson.toJSON exampleName `shouldBe` exampleNameJson

    it "decodes a Name from the pinned kebab-case-keyed JSON" do
      case Aeson.fromJSON exampleNameJson of
        Aeson.Error err -> expectationFailure err
        Aeson.Success decoded -> decoded `shouldBe` exampleName

    it "encodes an all-nullary sum (Primitive) as a bare kebab-case string, splitting before digits" do
      Aeson.toJSON Input.Int4Primitive `shouldBe` Aeson.String "int-4"

    it "encodes a non-nullary sum alternative (Scalar) as a single-field object keyed by the stripped, kebab-cased constructor" do
      Aeson.toJSON (Input.PrimitiveScalar Input.Int4Primitive)
        `shouldBe` [aesonQQ| { "primitive": "int-4" } |]

      Aeson.toJSON
        ( Input.CustomScalar
            Input.CustomTypeRef
              { name = exampleName,
                pgSchema = "public",
                pgName = "user_id",
                index = 0
              }
        )
        `shouldBe` Aeson.object
          [ ( "custom",
              Aeson.object
                [ ("name", exampleNameJson),
                  ("pg-schema", Aeson.toJSON @Text "public"),
                  ("pg-name", Aeson.toJSON @Text "user_id"),
                  ("index", Aeson.toJSON @Natural 0)
                ]
            )
          ]

    it "encodes CustomTypeRef with kebab-case keys" do
      Aeson.toJSON
        Input.CustomTypeRef
          { name = exampleName,
            pgSchema = "public",
            pgName = "user_id",
            index = 0
          }
        `shouldBe` Aeson.object
          [ ("name", exampleNameJson),
            ("pg-schema", Aeson.toJSON @Text "public"),
            ("pg-name", Aeson.toJSON @Text "user_id"),
            ("index", Aeson.toJSON @Natural 0)
          ]

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
        `shouldBe` Aeson.object
          [ ("name", exampleNameJson),
            ("raw-name", Aeson.toJSON @Text "raw"),
            ("param-index", Aeson.toJSON @Natural 1)
          ]

    it "encodes a composite custom type definition tagged by stripping the type name" do
      Aeson.toJSON (Input.CompositeCustomTypeDefinition [])
        `shouldBe` [aesonQQ| { "composite": [] } |]

    it "encodes an enum custom type definition tagged by stripping the type name, kebab-casing enum variant fields" do
      Aeson.toJSON (Input.EnumCustomTypeDefinition [Input.EnumVariant {name = exampleName, pgName = "raw_enum"}])
        `shouldBe` Aeson.object
          [ ( "enum",
              Aeson.toJSON
                [ Aeson.object
                    [ ("name", exampleNameJson),
                      ("pg-name", Aeson.toJSON @Text "raw_enum")
                    ]
                ]
            )
          ]

    it "encodes a domain custom type definition tagged by stripping the type name" do
      Aeson.toJSON
        ( Input.DomainCustomTypeDefinition
            Input.Value {dimensionality = 0, elementIsNullable = False, scalar = Input.PrimitiveScalar Input.Int4Primitive}
        )
        `shouldBe` [aesonQQ| { "domain": { "dimensionality": 0, "element-is-nullable": false, "scalar": { "primitive": "int-4" } } } |]

    it "encodes a CustomType record with kebab-cased pg-schema/pg-name fields" do
      Aeson.toJSON
        Input.CustomType
          { name = exampleName,
            pgSchema = "public",
            pgName = "status",
            definition = Input.EnumCustomTypeDefinition []
          }
        `shouldBe` Aeson.object
          [ ("name", exampleNameJson),
            ("pg-schema", Aeson.toJSON @Text "public"),
            ("pg-name", Aeson.toJSON @Text "status"),
            ("definition", Aeson.object [("enum", Aeson.Array mempty)])
          ]

    it "encodes a Value with kebab-cased dimensionality/element-is-nullable fields" do
      Aeson.toJSON
        Input.Value
          { dimensionality = 2,
            elementIsNullable = True,
            scalar = Input.PrimitiveScalar Input.Int4Primitive
          }
        `shouldBe` [aesonQQ|
          {
            "dimensionality": 2,
            "element-is-nullable": true,
            "scalar": {"primitive": "int-4"}
          }
        |]

exampleName :: Input.Name
exampleName =
  Input.Name
    { inCamelCase = "userId",
      inPascalCase = "UserId",
      inKebabCase = "user-id",
      inTrainCase = "User-Id",
      inScreamingKebabCase = "USER-ID",
      inSnakeCase = "user_id",
      inCamelSnakeCase = "User_Id",
      inScreamingSnakeCase = "USER_ID"
    }

exampleNameJson :: Aeson.Value
exampleNameJson =
  [aesonQQ|
    {
      "in-camel-case": "userId",
      "in-pascal-case": "UserId",
      "in-kebab-case": "user-id",
      "in-train-case": "User-Id",
      "in-screaming-kebab-case": "USER-ID",
      "in-snake-case": "user_id",
      "in-camel-snake-case": "User_Id",
      "in-screaming-snake-case": "USER_ID"
    }
  |]

project1Json :: Aeson.Value
project1Json =
  [aesonQQ|
    {
      "space": {
        "in-camel-case": "demo", "in-camel-snake-case": "Demo", "in-kebab-case": "demo",
        "in-pascal-case": "Demo", "in-screaming-kebab-case": "DEMO", "in-screaming-snake-case": "DEMO",
        "in-snake-case": "demo", "in-train-case": "Demo"
      },
      "name": {
        "in-camel-case": "demoProject", "in-camel-snake-case": "Demo_Project", "in-kebab-case": "demo-project",
        "in-pascal-case": "DemoProject", "in-screaming-kebab-case": "DEMO-PROJECT", "in-screaming-snake-case": "DEMO_PROJECT",
        "in-snake-case": "demo_project", "in-train-case": "Demo-Project"
      },
      "version": { "major": 1, "minor": 0, "patch": 0 },
      "custom-types": [
        {
          "name": {
            "in-camel-case": "status", "in-camel-snake-case": "Status", "in-kebab-case": "status",
            "in-pascal-case": "Status", "in-screaming-kebab-case": "STATUS", "in-screaming-snake-case": "STATUS",
            "in-snake-case": "status", "in-train-case": "Status"
          },
          "pg-schema": "public",
          "pg-name": "status",
          "definition": { "enum": [] }
        }
      ],
      "queries": [
        {
          "name": {
            "in-camel-case": "getUser", "in-camel-snake-case": "Get_User", "in-kebab-case": "get-user",
            "in-pascal-case": "GetUser", "in-screaming-kebab-case": "GET-USER", "in-screaming-snake-case": "GET_USER",
            "in-snake-case": "get_user", "in-train-case": "Get-User"
          },
          "src-path": "queries/get_user.sql",
          "identity": false,
          "idempotent": true,
          "params": [
            {
              "name": {
                "in-camel-case": "userId", "in-camel-snake-case": "User_Id", "in-kebab-case": "user-id",
                "in-pascal-case": "UserId", "in-screaming-kebab-case": "USER-ID", "in-screaming-snake-case": "USER_ID",
                "in-snake-case": "user_id", "in-train-case": "User-Id"
              },
              "pg-name": "user_id",
              "is-nullable": false,
              "value": { "dimensionality": 0, "element-is-nullable": false, "scalar": { "primitive": "int-4" } }
            }
          ],
          "result": {
            "rows": {
              "cardinality": "optional",
              "columns": [
                {
                  "name": {
                    "in-camel-case": "id", "in-camel-snake-case": "Id", "in-kebab-case": "id",
                    "in-pascal-case": "Id", "in-screaming-kebab-case": "ID", "in-screaming-snake-case": "ID",
                    "in-snake-case": "id", "in-train-case": "Id"
                  },
                  "pg-name": "id",
                  "is-nullable": false,
                  "value": {
                    "dimensionality": 0,
                    "element-is-nullable": false,
                    "scalar": { "primitive": "int-4" }
                  }
                },
                {
                  "name": {
                    "in-camel-case": "name", "in-camel-snake-case": "Name", "in-kebab-case": "name",
                    "in-pascal-case": "Name", "in-screaming-kebab-case": "NAME", "in-screaming-snake-case": "NAME",
                    "in-snake-case": "name", "in-train-case": "Name"
                  },
                  "pg-name": "name",
                  "is-nullable": false,
                  "value": {
                    "dimensionality": 0,
                    "element-is-nullable": false,
                    "scalar": { "primitive": "text" }
                  }
                },
                {
                  "name": {
                    "in-camel-case": "email", "in-camel-snake-case": "Email", "in-kebab-case": "email",
                    "in-pascal-case": "Email", "in-screaming-kebab-case": "EMAIL", "in-screaming-snake-case": "EMAIL",
                    "in-snake-case": "email", "in-train-case": "Email"
                  },
                  "pg-name": "email",
                  "is-nullable": true,
                  "value": {
                    "dimensionality": 0,
                    "element-is-nullable": false,
                    "scalar": { "primitive": "text" }
                  }
                },
                {
                  "name": {
                    "in-camel-case": "status", "in-camel-snake-case": "Status", "in-kebab-case": "status",
                    "in-pascal-case": "Status", "in-screaming-kebab-case": "STATUS", "in-screaming-snake-case": "STATUS",
                    "in-snake-case": "status", "in-train-case": "Status"
                  },
                  "pg-name": "status",
                  "is-nullable": false,
                  "value": {
                    "dimensionality": 0,
                    "element-is-nullable": false,
                    "scalar": {
                      "custom": {
                        "name": {
                          "in-camel-case": "status", "in-camel-snake-case": "Status", "in-kebab-case": "status",
                          "in-pascal-case": "Status", "in-screaming-kebab-case": "STATUS", "in-screaming-snake-case": "STATUS",
                          "in-snake-case": "status", "in-train-case": "Status"
                        },
                        "pg-schema": "public",
                        "pg-name": "status",
                        "index": 0
                      }
                    }
                  }
                }
              ]
            }
          },
          "fragments": [
            { "sql": "SELECT id, name, email, status FROM users WHERE id = " },
            { "var": { "name": {
                "in-camel-case": "userId", "in-camel-snake-case": "User_Id", "in-kebab-case": "user-id",
                "in-pascal-case": "UserId", "in-screaming-kebab-case": "USER-ID", "in-screaming-snake-case": "USER_ID",
                "in-snake-case": "user_id", "in-train-case": "User-Id"
              }, "raw-name": "user_id", "param-index": 1 } }
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
