-- |
-- Detects custom Postgres types whose normalized identifiers collide despite
-- having distinct Postgres identities (schema-qualified names) — e.g.
-- @"foo_bar"@ and @"foo-bar"@ both normalize to the @foo_bar@ identifier
-- generators consume.
module Logic.Domain.CustomTypeUniqueness (findDuplicateGroups, spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Gen qualified
import Test.Hspec
import Utils.Prelude

-- | Groups custom types by their normalized @inSnakeCase@ identifier,
-- returning only the groups with more than one distinct Postgres identity.
findDuplicateGroups :: [Gen.CustomType] -> [NonEmpty Gen.CustomType]
findDuplicateGroups customTypes =
  customTypes
    & foldr (\x -> Map.insertWith (<>) x.name.inSnakeCase (x :| [])) Map.empty
    & Map.elems
    & filter (\group_ -> length (toList group_) > 1)

spec :: Spec
spec = do
  describe "findDuplicateGroups" do
    it "finds no collisions when all identifiers are distinct" do
      findDuplicateGroups [customType "public" "foo_bar", customType "public" "baz"]
        `shouldSatisfy` null

    it "finds a collision between separator-style variants" do
      let result = findDuplicateGroups [customType "public" "foo_bar", customType "public" "foo-bar"]
      map (map (.pgName) . toList) result `shouldBe` [["foo_bar", "foo-bar"]]

    it "flags a collision even across distinct schemas, since the identifier handed to generators drops the schema" do
      findDuplicateGroups [customType "a" "foo_bar", customType "b" "foo_bar"]
        `shouldSatisfy` (not . null)
  where
    customType :: Text -> Text -> Gen.CustomType
    customType pgSchema pgName =
      Gen.CustomType
        { name = nameOf pgName,
          pgSchema = pgSchema,
          pgName = pgName,
          definition = Gen.EnumCustomTypeDefinition []
        }

    nameOf :: Text -> Gen.Name
    nameOf pgName =
      Gen.Name
        { inCamelCase = pgName,
          inPascalCase = pgName,
          inKebabCase = pgName,
          inTrainCase = pgName,
          inScreamingKebabCase = pgName,
          inSnakeCase = normalize pgName,
          inCamelSnakeCase = pgName,
          inScreamingSnakeCase = pgName
        }

    normalize :: Text -> Text
    normalize = Text.map (\c -> if c == '-' then '_' else c)
