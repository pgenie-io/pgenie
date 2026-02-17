module Logic.Name
  ( Name,
    toGenName,
    toPartsVector,
    toPartsNonEmpty,
    toText,
    tryFromText,
    inKebabCase,
    inSnakeCase,
    inCamelCase,
    inPascalCase,
    inScreamCase,
    megaparsecOf,
  )
where

import Base.Prelude
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Logic.Name.Megaparsec qualified as Megaparsec
import PGenieGen.Model.Input qualified as Gen
import Test.QuickCheck qualified as Qc
import TextBuilder qualified

-- |
-- Normalized name. Sequence of lowercase words separated by hyphens.
-- Each word must start with a lowercase latin letter and can contain lowercase latin letters and digits.
-- Must contain at least one word.
--
-- It can be converted to various casing formats, such as camelCase, PascalCase, snake_case, etc.
newtype Name = Name (Vector Text)
  deriving newtype (Eq, Ord, Hashable, Semigroup, Monoid)

instance Arbitrary Name where
  arbitrary = do
    parts <- Qc.listOf1 do
      firstChar <-
        Qc.elements ['a' .. 'z']
      restChars <-
        Qc.listOf
          ( Qc.oneof
              [ Qc.elements ['a' .. 'z'],
                Qc.arbitrary `Qc.suchThat` isDigit
              ]
          )

      return (Text.pack (firstChar : restChars))

    return (Name (Vector.fromList parts))

  shrink (Name parts) =
    case Vector.uncons parts of
      Nothing -> []
      Just (firstPart, restParts) -> do
        shrunkFirstPart <- shrinkPart firstPart
        shrunkRestParts <- Qc.shrinkList shrinkPart (Vector.toList restParts)
        return (Name (Vector.fromList (shrunkFirstPart : shrunkRestParts)))
    where
      shrinkPart part =
        case Text.uncons part of
          Nothing -> []
          Just (firstChar, restChars) -> do
            shrunkRestChars <- Qc.shrink restChars
            let shrunkPart = Text.cons firstChar shrunkRestChars
            return shrunkPart

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

toPartsVector :: Name -> Vector Text
toPartsVector (Name parts) = parts

toPartsNonEmpty :: Name -> NonEmpty Text
toPartsNonEmpty (Name parts) =
  case Vector.toList parts of
    [] -> error "Name.toPartsNonEmpty: empty name"
    x : xs -> x :| xs

toText :: Name -> Text
toText = to @Text . toTextBuilder

toTextBuilder :: Name -> TextBuilder
toTextBuilder = toTextBuilderInSnakeCase

toTextBuilderInKebabCase :: Name -> TextBuilder
toTextBuilderInKebabCase = TextBuilder.intercalateMap "-" to . toPartsVector

toTextBuilderInCamelCase :: Name -> TextBuilder
toTextBuilderInCamelCase = fromList . Vector.toList . toPartsVector
  where
    fromList = \case
      [] -> mempty
      x : xs -> to x <> foldMap (to . Text.toTitle) xs

toTextBuilderInPascalCase :: Name -> TextBuilder
toTextBuilderInPascalCase = foldMap (to . Text.toTitle) . Vector.toList . toPartsVector

toTextBuilderInSnakeCase :: Name -> TextBuilder
toTextBuilderInSnakeCase = TextBuilder.intercalateMap "_" to . toPartsVector

toTextBuilderInScreamCase :: Name -> TextBuilder
toTextBuilderInScreamCase = TextBuilder.intercalateMap "_" (to . Text.toUpper) . toPartsVector

toGenName :: Name -> Gen.Name
toGenName name =
  case toPartsNonEmpty name of
    word :| rest ->
      Gen.Name
        { head = wordToGenWord word,
          tail = map wordOrNumberFromPart rest
        }
  where
    wordToGenWord :: Text -> Gen.Word
    wordToGenWord = fromMaybe (error "Empty word") . nonEmpty . fmap textCharToWordChar . Text.unpack

    wordOrNumberFromPart :: Text -> Gen.WordOrNumber
    wordOrNumberFromPart text =
      case reads (Text.unpack text) of
        [(n, "")] -> Gen.WordOrNumberNumber n
        _ -> Gen.WordOrNumberWord (wordToGenWord text)

    textCharToWordChar :: Char -> Gen.WordChar
    textCharToWordChar = \case
      'a' -> Gen.WordCharA
      'b' -> Gen.WordCharB
      'c' -> Gen.WordCharC
      'd' -> Gen.WordCharD
      'e' -> Gen.WordCharE
      'f' -> Gen.WordCharF
      'g' -> Gen.WordCharG
      'h' -> Gen.WordCharH
      'i' -> Gen.WordCharI
      'j' -> Gen.WordCharJ
      'k' -> Gen.WordCharK
      'l' -> Gen.WordCharL
      'm' -> Gen.WordCharM
      'n' -> Gen.WordCharN
      'o' -> Gen.WordCharO
      'p' -> Gen.WordCharP
      'q' -> Gen.WordCharQ
      'r' -> Gen.WordCharR
      's' -> Gen.WordCharS
      't' -> Gen.WordCharT
      'u' -> Gen.WordCharU
      'v' -> Gen.WordCharV
      'w' -> Gen.WordCharW
      'x' -> Gen.WordCharX
      'y' -> Gen.WordCharY
      'z' -> Gen.WordCharZ
      c -> error ("Invalid character in name: " <> show c)

tryFromText :: Text -> Either Text Name
tryFromText text =
  let normalized = normalizeInput text
   in fmap Name . fmap Vector.fromList . Megaparsec.toTextParser (Megaparsec.complete Megaparsec.parts) $ normalized
  where
    -- | Normalize input text to snake_case format.
    -- Converts to lowercase and replaces hyphens with underscores.
    normalizeInput :: Text -> Text
    normalizeInput = Text.toLower . Text.replace "-" "_"

megaparsecOf :: Megaparsec.Parser Name
megaparsecOf = fmap (Name . Vector.fromList) Megaparsec.parts

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
