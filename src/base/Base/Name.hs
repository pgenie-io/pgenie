module Base.Name
  ( Name,
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

import Base.Name.Megaparsec qualified as Megaparsec
import Base.Prelude
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import TextBuilder qualified

-- |
-- Normalized name. Sequence of lowercase words separated by hyphens.
-- Each word must start with a lowercase latin letter and can contain lowercase latin letters and digits.
-- Must contain at least one word.
--
-- It can be converted to various casing formats, such as camelCase, PascalCase, snake_case, etc.
newtype Name = Name (Vector Text)
  deriving newtype (Eq, Ord, Hashable, Semigroup, Monoid)

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
toTextBuilder = TextBuilder.intercalateMap "-" to . toPartsVector

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

tryFromText :: Text -> Either Text Name
tryFromText =
  fmap Name . fmap Vector.fromList . Megaparsec.toTextParser (Megaparsec.complete Megaparsec.parts)

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
