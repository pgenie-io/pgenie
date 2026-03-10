module PGenieGen.Model.Input.Name where

import Data.Aeson qualified as Aeson
import Dhall qualified
import PGenieGen.Dhall.Orphans ()
import PGenieGen.Model.Input.Word (Word (..))
import PGenieGen.Model.Input.WordOrNumber (WordOrNumber (..))
import PGenieGen.Prelude hiding (Version, Word)

-- | A strict name with a head word and tail of words or numbers
data Name = Name
  { head :: Word,
    tail :: [WordOrNumber]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Aeson.ToJSON Name where
  toJSON (Name head tail) =
    Aeson.Array
      $ fromList (Aeson.toJSON head : map Aeson.toJSON tail)

instance Aeson.FromJSON Name where
  parseJSON = \case
    Aeson.Array array ->
      case toList array of
        [] -> fail "Expected non-empty array for Name"
        (headValue : tailValues) -> do
          head <- Aeson.parseJSON headValue
          tail <- mapM Aeson.parseJSON tailValues
          return (Name head tail)
    _ -> fail "Expected array for Name"
