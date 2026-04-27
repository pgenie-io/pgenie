module Logic.Features.Naming.Types.Name
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
import Logic.Features.Naming.Types.Name.Megaparsec qualified as Megaparsec
import PGenieGen.Model.Input qualified as Gen
import Test.Hspec
import Test.QuickCheck qualified as Qc
import TextBuilder qualified
import Utils.Prelude

-- |
-- Normalized name backed by the gen-sdk model.
--
-- A name consists of a lowercase head word followed by lowercase words or
-- natural numbers, separated by underscores. Parsing normalizes letter case,
-- treats hyphens as separators, and splits adjacent letter and digit runs into
-- separate parts.
--
-- It can be converted to various casing formats, such as camelCase, PascalCase, snake_case, etc.
newtype Name = Name Gen.Name
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
toPartsNonEmpty (Name name) =
  wordToText name.head :| map tailPartToText name.tail
  where
    tailPartToText = \case
      Gen.WordOrNumberWord word -> wordToText word
      Gen.WordOrNumberNumber number -> Text.pack (show number)

    wordToText :: Gen.Word -> Text
    wordToText (Gen.Word chars) =
      Text.pack (map wordCharToChar (toList chars))
      where
        wordCharToChar :: Gen.WordChar -> Char
        wordCharToChar = \case
          Gen.WordCharA -> 'a'
          Gen.WordCharB -> 'b'
          Gen.WordCharC -> 'c'
          Gen.WordCharD -> 'd'
          Gen.WordCharE -> 'e'
          Gen.WordCharF -> 'f'
          Gen.WordCharG -> 'g'
          Gen.WordCharH -> 'h'
          Gen.WordCharI -> 'i'
          Gen.WordCharJ -> 'j'
          Gen.WordCharK -> 'k'
          Gen.WordCharL -> 'l'
          Gen.WordCharM -> 'm'
          Gen.WordCharN -> 'n'
          Gen.WordCharO -> 'o'
          Gen.WordCharP -> 'p'
          Gen.WordCharQ -> 'q'
          Gen.WordCharR -> 'r'
          Gen.WordCharS -> 's'
          Gen.WordCharT -> 't'
          Gen.WordCharU -> 'u'
          Gen.WordCharV -> 'v'
          Gen.WordCharW -> 'w'
          Gen.WordCharX -> 'x'
          Gen.WordCharY -> 'y'
          Gen.WordCharZ -> 'z'

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

toGenName :: Name -> Gen.Name
toGenName (Name name) = name

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
  headWord <- wordTextToGenWordMaybe firstPart
  tailWords <- traverse tailPartToGenWordOrNumberMaybe tailParts
  pure
    ( Name
        Gen.Name
          { head = headWord,
            tail = tailWords
          }
    )
  where
    tailPartToGenWordOrNumberMaybe :: Text -> Maybe Gen.WordOrNumber
    tailPartToGenWordOrNumberMaybe text =
      case readMaybe (Text.unpack text) of
        Just number -> Just (Gen.WordOrNumberNumber number)
        Nothing -> Gen.WordOrNumberWord <$> wordTextToGenWordMaybe text

wordTextToGenWordMaybe :: Text -> Maybe Gen.Word
wordTextToGenWordMaybe text = do
  chars <- traverse wordCharFromCharMaybe (Text.unpack text)
  Gen.Word <$> nonEmpty chars
  where
    wordCharFromCharMaybe :: Char -> Maybe Gen.WordChar
    wordCharFromCharMaybe = \case
      'a' -> Just Gen.WordCharA
      'b' -> Just Gen.WordCharB
      'c' -> Just Gen.WordCharC
      'd' -> Just Gen.WordCharD
      'e' -> Just Gen.WordCharE
      'f' -> Just Gen.WordCharF
      'g' -> Just Gen.WordCharG
      'h' -> Just Gen.WordCharH
      'i' -> Just Gen.WordCharI
      'j' -> Just Gen.WordCharJ
      'k' -> Just Gen.WordCharK
      'l' -> Just Gen.WordCharL
      'm' -> Just Gen.WordCharM
      'n' -> Just Gen.WordCharN
      'o' -> Just Gen.WordCharO
      'p' -> Just Gen.WordCharP
      'q' -> Just Gen.WordCharQ
      'r' -> Just Gen.WordCharR
      's' -> Just Gen.WordCharS
      't' -> Just Gen.WordCharT
      'u' -> Just Gen.WordCharU
      'v' -> Just Gen.WordCharV
      'w' -> Just Gen.WordCharW
      'x' -> Just Gen.WordCharX
      'y' -> Just Gen.WordCharY
      'z' -> Just Gen.WordCharZ
      _ -> Nothing

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
