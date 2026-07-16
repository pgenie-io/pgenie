-- |
-- A representative 'Contract.Project' value — a single-query, single-migration
-- "demo" project — shared across the gen-contract-v4 specs to exercise Aeson
-- round-tripping and Dhall round-tripping against the pinned v4 contract.
module GenContractV4.Fixtures.Fixture1
  ( input,
  )
where

import Cases qualified
import Data.List.NonEmpty qualified as NonEmpty
import GenContractV4.Contract qualified as Contract
import Utils.Prelude

input :: Contract.Project
input =
  Contract.Project
    { space = textName "demo",
      name = textName "demo_project",
      version = Contract.Version {major = 1, minor = 0, patch = 0},
      customTypes = [],
      queries = [exampleQuery],
      migrations =
        [ Contract.Migration
            { name = "001_create_users",
              sql = "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT);"
            }
        ]
    }
  where
    -- Helper function to create a name from text with all case renderings
    textName :: Text -> Contract.Name
    textName source =
      Contract.Name
        { inCamelCase = Cases.camelize source,
          inPascalCase = Cases.process Cases.title Cases.camel source,
          inKebabCase = Cases.spinalize source,
          inTrainCase = Cases.process Cases.title Cases.spinal source,
          inScreamingKebabCase = Cases.process Cases.upper Cases.spinal source,
          inSnakeCase = Cases.snakify source,
          inCamelSnakeCase = Cases.process Cases.title Cases.snake source,
          inScreamingSnakeCase = Cases.process Cases.upper Cases.snake source
        }

    -- Example query
    exampleQuery :: Contract.Query
    exampleQuery =
      Contract.Query
        { name = textName "get_user",
          srcPath = "queries/get_user.sql",
          identity = False,
          idempotent = True,
          params = [userIdParam],
          result = userResult,
          fragments =
            [ Contract.SqlQueryFragment "SELECT id, name, email FROM users WHERE id = ",
              Contract.VarQueryFragment
                ( Contract.Var
                    { name = textName "user_id",
                      rawName = "user_id",
                      paramIndex = 1
                    }
                )
            ]
        }

    -- Parameter for user ID
    userIdParam :: Contract.Member
    userIdParam =
      Contract.Member
        { name = textName "user_id",
          pgName = "user_id",
          isNullable = False,
          value =
            Contract.Value
              { arraySettings = Nothing,
                scalar = Contract.PrimitiveScalar Contract.Int4Primitive
              }
        }

    -- Result structure for user query
    userResult :: Contract.Result
    userResult =
      Contract.RowsResult
        Contract.ResultRows
          { cardinality = Contract.OptionalResultRowsCardinality,
            columns =
              NonEmpty.fromList
                [ Contract.Member
                    { name = textName "id",
                      pgName = "id",
                      isNullable = False,
                      value =
                        Contract.Value
                          { arraySettings = Nothing,
                            scalar = Contract.PrimitiveScalar Contract.Int4Primitive
                          }
                    },
                  Contract.Member
                    { name = textName "name",
                      pgName = "name",
                      isNullable = False,
                      value =
                        Contract.Value
                          { arraySettings = Nothing,
                            scalar = Contract.PrimitiveScalar Contract.TextPrimitive
                          }
                    },
                  Contract.Member
                    { name = textName "email",
                      pgName = "email",
                      isNullable = True,
                      value =
                        Contract.Value
                          { arraySettings = Nothing,
                            scalar = Contract.PrimitiveScalar Contract.TextPrimitive
                          }
                    }
                ]
          }
