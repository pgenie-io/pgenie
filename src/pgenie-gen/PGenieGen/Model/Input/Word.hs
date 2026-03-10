module PGenieGen.Model.Input.Word where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Dhall qualified
import PGenieGen.Dhall.Orphans ()
import PGenieGen.Model.Input.WordChar qualified as WordChar
import PGenieGen.Prelude hiding (Version, Word)

-- | A word is a non-empty list of characters
newtype Word = Word (NonEmpty WordChar.WordChar)
  deriving stock (Generic)
  deriving newtype (Show, Eq, Dhall.FromDhall, Dhall.ToDhall)

toChars :: Word -> NonEmpty Char
toChars (Word wordChars) =
  fmap WordChar.toChar wordChars

toText :: Word -> Text
toText =
  Text.pack . toList . toChars

instance Aeson.ToJSON Word where
  toJSON =
    Aeson.toJSON . toText

instance Aeson.FromJSON Word where
  parseJSON = \case
    Aeson.String text -> do
      chars <- forM (Text.unpack text) \char ->
        case WordChar.maybeFromChar char of
          Nothing -> fail ("Invalid character in Word: " <> show char)
          Just wordChar -> pure wordChar
      nonEmptyChars <- case chars of
        [] -> fail "Word cannot be empty"
        (char : chars) -> pure (char :| chars)
      pure (Word nonEmptyChars)
    _ ->
      fail "Expected a JSON string for Word"
