module Logic.Domain.Name
  ( Name,
    toGenName,
    toText,
    tryFromText,
    inKebabCase,
    inSnakeCase,
    inCamelCase,
    inPascalCase,
    inScreamCase,
    megaparsecOf,
    spec,
  )
where

import Data.Text qualified as Text
import Gen qualified
import Logic.Domain.Name.Megaparsec qualified as Megaparsec
import Test.Hspec
import Test.QuickCheck qualified as Qc
import TextBuilder qualified
import Utils.Prelude

-- |
-- Normalized name consisting of lowercase word parts separated by underscores.
--
-- A name consists of a lowercase head word followed by lowercase words or
-- natural numbers, separated by underscores. Parsing normalizes letter case,
-- treats hyphens as separators, and splits adjacent letter and digit runs into
-- separate parts.
--
-- It can be converted to various casing formats, such as camelCase, PascalCase, snake_case, etc.
newtype Name = Name (NonEmpty Text)
  deriving newtype (Eq)

instance Ord Name where
  compare = comparing toText

instance Hashable Name where
  hashWithSalt salt = hashWithSalt salt . toText

instance Arbitrary Name where
  arbitrary = do
    firstPart <- arbitraryWordPart
    restParts <- Qc.listOf arbitraryTailPart
    case partsToNameMaybe (firstPart :| restParts) of
      Just name -> pure name
      Nothing -> error "Name.arbitrary: generated invalid normalized name"
    where
      arbitraryWordPart :: Qc.Gen Text
      arbitraryWordPart = do
        firstChar <- Qc.elements ['a' .. 'z']
        restChars <- Qc.listOf (Qc.elements ['a' .. 'z'])
        pure (Text.pack (firstChar : restChars))

      arbitraryTailPart :: Qc.Gen Text
      arbitraryTailPart =
        Qc.oneof
          [ arbitraryWordPart,
            Text.pack . show <$> Qc.chooseInt (0, 1000000)
          ]

  shrink name =
    case toPartsNonEmpty name of
      firstPart :| restParts -> do
        shrunkFirstPart <- shrinkWordPart firstPart
        shrunkRestParts <- Qc.shrinkList shrinkTailPart restParts
        maybeToList (partsToNameMaybe (shrunkFirstPart :| shrunkRestParts))
    where
      shrinkWordPart :: Text -> [Text]
      shrinkWordPart =
        filter isWordPart . Qc.shrink

      shrinkTailPart :: Text -> [Text]
      shrinkTailPart =
        filter isTailPart . Qc.shrink

      isTailPart :: Text -> Bool
      isTailPart part =
        isWordPart part || (not (Text.null part) && Text.all isDigit part)

      isWordPart :: Text -> Bool
      isWordPart part =
        not (Text.null part) && Text.all isAsciiLower part

instance Show Name where
  showsPrec p = showsPrec p . toText

instance IsString Name where
  fromString = either (error . Text.unpack) id . tryFromText . Text.pack

instance IsSome Text Name where
  to = toText
  maybeFrom = either (const Nothing) Just . tryFromText

instance IsSome LazyText Name where
  to = to . toText
  maybeFrom = either (const Nothing) Just . tryFromText . to

instance IsSome TextBuilder Name where
  to = toTextBuilder
  maybeFrom = either (const Nothing) Just . tryFromText . to

toPartsNonEmpty :: Name -> NonEmpty Text
toPartsNonEmpty (Name parts) = parts

toText :: Name -> Text
toText = to @Text . toTextBuilder

toTextBuilder :: Name -> TextBuilder
toTextBuilder = toTextBuilderInSnakeCase

toTextBuilderInKebabCase :: Name -> TextBuilder
toTextBuilderInKebabCase = TextBuilder.intercalateMap "-" to . toPartsNonEmpty

toTextBuilderInCamelCase :: Name -> TextBuilder
toTextBuilderInCamelCase name =
  case toPartsNonEmpty name of
    x :| xs -> to x <> foldMap (to . Text.toTitle) xs

toTextBuilderInPascalCase :: Name -> TextBuilder
toTextBuilderInPascalCase = foldMap (to . Text.toTitle) . toPartsNonEmpty

toTextBuilderInSnakeCase :: Name -> TextBuilder
toTextBuilderInSnakeCase = TextBuilder.intercalateMap "_" to . toPartsNonEmpty

toTextBuilderInScreamCase :: Name -> TextBuilder
toTextBuilderInScreamCase = TextBuilder.intercalateMap "_" (to . Text.toUpper) . toPartsNonEmpty

toTextBuilderInTrainCase :: Name -> TextBuilder
toTextBuilderInTrainCase = TextBuilder.intercalateMap "-" (to . Text.toTitle) . toPartsNonEmpty

toTextBuilderInScreamingKebabCase :: Name -> TextBuilder
toTextBuilderInScreamingKebabCase = TextBuilder.intercalateMap "-" (to . Text.toUpper) . toPartsNonEmpty

toTextBuilderInCamelSnakeCase :: Name -> TextBuilder
toTextBuilderInCamelSnakeCase = TextBuilder.intercalateMap "_" (to . Text.toTitle) . toPartsNonEmpty

toGenName :: Name -> Gen.Name
toGenName name =
  Gen.Name
    { inCamelCase = to @Text (toTextBuilderInCamelCase name),
      inPascalCase = to @Text (toTextBuilderInPascalCase name),
      inKebabCase = to @Text (toTextBuilderInKebabCase name),
      inTrainCase = to @Text (toTextBuilderInTrainCase name),
      inScreamingKebabCase = to @Text (toTextBuilderInScreamingKebabCase name),
      inSnakeCase = to @Text (toTextBuilderInSnakeCase name),
      inCamelSnakeCase = to @Text (toTextBuilderInCamelSnakeCase name),
      inScreamingSnakeCase = to @Text (toTextBuilderInScreamCase name)
    }

tryFromText :: Text -> Either Text Name
tryFromText text =
  Megaparsec.toTextParser (Megaparsec.complete megaparsecOf) text

megaparsecOf :: Megaparsec.Parser Name
megaparsecOf = do
  parts <- Megaparsec.parts
  case nonEmpty parts >>= partsToNameMaybe of
    Just name -> pure name
    Nothing -> fail "Invalid normalized name"

-- * Rendering

inKebabCase :: (IsSome a TextBuilder) => Name -> a
inKebabCase = to . toTextBuilderInKebabCase

inCamelCase :: (IsSome a TextBuilder) => Name -> a
inCamelCase = to . toTextBuilderInCamelCase

inPascalCase :: (IsSome a TextBuilder) => Name -> a
inPascalCase = to . toTextBuilderInPascalCase

inSnakeCase :: (IsSome a TextBuilder) => Name -> a
inSnakeCase = to . toTextBuilderInSnakeCase

inScreamCase :: (IsSome a TextBuilder) => Name -> a
inScreamCase = to . toTextBuilderInScreamCase

partsToNameMaybe :: NonEmpty Text -> Maybe Name
partsToNameMaybe (firstPart :| tailParts) = do
  guard (isWordPart firstPart)
  traverse_ (\p -> guard (isWordPart p || isNumberPart p)) tailParts
  pure (Name (firstPart :| tailParts))
  where
    isWordPart part =
      not (Text.null part) && Text.all isAsciiLower part
    isNumberPart part =
      not (Text.null part) && Text.all isDigit part

-- | Test suite for parsing and rendering normalized names.
spec :: Spec
spec = do
  describe "tryFromText" do
    it "parses lowercase snake_case names" do
      let result = tryFromText "album_format"
      result `shouldSatisfy` isRight

    it "parses uppercase single words" do
      let result = tryFromText "Vinyl"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "vinyl"

    it "parses uppercase with hyphens" do
      let result = tryFromText "DVD-Audio"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "dvd_audio"

    it "parses all-caps names" do
      let result = tryFromText "SACD"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "sacd"

    it "parses CD correctly" do
      let result = tryFromText "CD"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "cd"

    it "parses PascalCase names" do
      let result = tryFromText "Digital"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "digital"

    it "preserves lowercase names" do
      let result = tryFromText "cassette"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "cassette"

    it "handles mixed case correctly" do
      let result = tryFromText "Cassette"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "cassette"

    it "handles kebab-case by converting to snake_case" do
      let result = tryFromText "album-format"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "album_format"

    it "parses numeric tail parts" do
      let result = tryFromText "album_2024"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "album_2024"

    it "treats mixed alphanumeric parts as multiple parts" do
      tryFromText "box2d" `shouldBe` Right "box_2_d"
      tryFromText "float8" `shouldBe` Right "float_8"
