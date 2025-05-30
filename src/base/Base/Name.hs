module Base.Name
  ( Name,
    toParts,
    toText,
    tryFromText,
    inKebabCase,
    inSnakeCase,
    inCamelCase,
    inPascalCase,
    inScreamCase,
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

toParts :: Name -> Vector Text
toParts (Name parts) = parts

toText :: Name -> Text
toText = to @Text . toTextBuilder

toTextBuilder :: Name -> TextBuilder
toTextBuilder = TextBuilder.intercalateMap "-" to . toParts

toTextBuilderInKebabCase :: Name -> TextBuilder
toTextBuilderInKebabCase = TextBuilder.intercalateMap "-" to . toParts

toTextBuilderInCamelCase :: Name -> TextBuilder
toTextBuilderInCamelCase = fromList . Vector.toList . toParts
  where
    fromList = \case
      [] -> mempty
      x : xs -> to x <> foldMap (to . Text.toTitle) xs

toTextBuilderInPascalCase :: Name -> TextBuilder
toTextBuilderInPascalCase = foldMap (to . Text.toTitle) . Vector.toList . toParts

toTextBuilderInSnakeCase :: Name -> TextBuilder
toTextBuilderInSnakeCase = TextBuilder.intercalateMap "_" to . toParts

toTextBuilderInScreamCase :: Name -> TextBuilder
toTextBuilderInScreamCase = TextBuilder.intercalateMap "_" (to . Text.toUpper) . toParts

tryFromText :: Text -> Either Text Name
tryFromText =
  fmap Name . fmap Vector.fromList . Megaparsec.toTextParser (Megaparsec.complete Megaparsec.parts)

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
