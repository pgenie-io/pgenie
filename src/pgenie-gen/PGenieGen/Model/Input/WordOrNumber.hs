module PGenieGen.Model.Input.WordOrNumber where

import Data.Aeson qualified as Aeson
import Dhall qualified
import PGenieGen.Dhall.Deriving qualified as Dhall.Deriving
import PGenieGen.Dhall.Orphans ()
import PGenieGen.Model.Input.Word (Word (..))
import PGenieGen.Prelude hiding (Version, Word)

-- | Either a word or a number
data WordOrNumber
  = WordOrNumberWord Word
  | WordOrNumberNumber Natural
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "WordOrNumber") WordOrNumber)

instance Aeson.ToJSON WordOrNumber where
  toJSON = \case
    WordOrNumberWord word -> Aeson.toJSON word
    WordOrNumberNumber number -> Aeson.toJSON number

instance Aeson.FromJSON WordOrNumber where
  parseJSON value =
    asum
      [ WordOrNumberWord <$> Aeson.parseJSON value,
        WordOrNumberNumber <$> Aeson.parseJSON value
      ]
