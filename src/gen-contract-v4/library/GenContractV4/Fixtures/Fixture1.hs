-- |
-- A representative 'Input.Project' value — a single-query, single-migration
-- "demo" project — shared across the gen-contract-v4 specs to exercise Aeson
-- round-tripping and Dhall round-tripping against the pinned v4 contract.
module GenContractV4.Fixtures.Fixture1
  ( input,
  )
where

import Cases qualified
import Data.List.NonEmpty qualified as NonEmpty
import GenContractV4.Input qualified as Input
import Utils.Prelude

input :: Input.Project
input =
  Input.Project
    { space = textName "demo",
      name = textName "demo_project",
      version = Input.Version {major = 1, minor = 0, patch = 0},
      customTypes = [],
      queries = [exampleQuery],
      migrations =
        [ Input.Migration
            { name = "001_create_users",
              sql = "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT);"
            }
        ]
    }
  where
    -- Helper function to create a name from text with all case renderings
    textName :: Text -> Input.Name
    textName source =
      Input.Name
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
    exampleQuery :: Input.Query
    exampleQuery =
      Input.Query
        { name = textName "get_user",
          srcPath = "queries/get_user.sql",
          identity = False,
          idempotent = True,
          params = [userIdParam],
          result = userResult,
          fragments =
            [ Input.SqlQueryFragment "SELECT id, name, email FROM users WHERE id = ",
              Input.VarQueryFragment
                ( Input.Var
                    { name = textName "user_id",
                      rawName = "user_id",
                      paramIndex = 1
                    }
                )
            ]
        }

    -- Parameter for user ID
    userIdParam :: Input.Member
    userIdParam =
      Input.Member
        { name = textName "user_id",
          pgName = "user_id",
          isNullable = False,
          value =
            Input.Value
              { arraySettings = Nothing,
                scalar = Input.PrimitiveScalar Input.Int4Primitive
              }
        }

    -- Result structure for user query
    userResult :: Input.Result
    userResult =
      Input.RowsResult
        Input.ResultRows
          { cardinality = Input.OptionalResultRowsCardinality,
            columns =
              NonEmpty.fromList
                [ Input.Member
                    { name = textName "id",
                      pgName = "id",
                      isNullable = False,
                      value =
                        Input.Value
                          { arraySettings = Nothing,
                            scalar = Input.PrimitiveScalar Input.Int4Primitive
                          }
                    },
                  Input.Member
                    { name = textName "name",
                      pgName = "name",
                      isNullable = False,
                      value =
                        Input.Value
                          { arraySettings = Nothing,
                            scalar = Input.PrimitiveScalar Input.TextPrimitive
                          }
                    },
                  Input.Member
                    { name = textName "email",
                      pgName = "email",
                      isNullable = True,
                      value =
                        Input.Value
                          { arraySettings = Nothing,
                            scalar = Input.PrimitiveScalar Input.TextPrimitive
                          }
                    }
                ]
          }
