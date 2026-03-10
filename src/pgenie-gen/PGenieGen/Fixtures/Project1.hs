-- | Integration with generator adapters.
module PGenieGen.Fixtures.Project1 where

import Data.List.NonEmpty qualified as NonEmpty
import PGenieGen.Model
import PGenieGen.Model.Input qualified as Input
import PGenieGen.Prelude

input :: Input
input =
  Input.Project
    { space = textName "demo",
      name = textName "demo_project",
      version = Input.Version {major = 1, minor = 0, patch = 0},
      customTypes = [],
      queries = [exampleQuery]
    }
  where
    -- Helper function to create a simple name from text
    textName :: Text -> Input.Name
    textName _text =
      Input.Name
        { head = Input.Word (NonEmpty.fromList [Input.WordCharA, Input.WordCharB]),
          tail = []
        }

    -- Example query
    exampleQuery :: Input.Query
    exampleQuery =
      Input.Query
        { name = textName "get_user",
          srcPath = "queries/get_user.sql",
          params = [userIdParam],
          result = Just userResult,
          fragments =
            [ Input.QueryFragmentSql "SELECT id, name, email FROM users WHERE id = ",
              Input.QueryFragmentVar
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
                scalar = Input.ScalarPrimitive Input.PrimitiveInt4
              }
        }

    -- Result structure for user query
    userResult :: Input.ResultRows
    userResult =
      Input.ResultRows
        { cardinality = Input.ResultRowsCardinalityOptional,
          columns =
            NonEmpty.fromList
              [ Input.Member
                  { name = textName "id",
                    pgName = "id",
                    isNullable = False,
                    value =
                      Input.Value
                        { arraySettings = Nothing,
                          scalar = Input.ScalarPrimitive Input.PrimitiveInt4
                        }
                  },
                Input.Member
                  { name = textName "name",
                    pgName = "name",
                    isNullable = False,
                    value =
                      Input.Value
                        { arraySettings = Nothing,
                          scalar = Input.ScalarPrimitive Input.PrimitiveText
                        }
                  },
                Input.Member
                  { name = textName "email",
                    pgName = "email",
                    isNullable = True,
                    value =
                      Input.Value
                        { arraySettings = Nothing,
                          scalar = Input.ScalarPrimitive Input.PrimitiveText
                        }
                  }
              ]
        }
