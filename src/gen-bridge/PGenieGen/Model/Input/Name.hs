module PGenieGen.Model.Input.Name where

import Dhall qualified
import PGenieGen.Dhall.Orphans ()
import PGenieGen.Prelude

-- | A name with precomputed case renderings
data Name = Name
  { inCamelCase :: Text,
    inPascalCase :: Text,
    inKebabCase :: Text,
    inTrainCase :: Text,
    inScreamingKebabCase :: Text,
    inSnakeCase :: Text,
    inCamelSnakeCase :: Text,
    inScreamingSnakeCase :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)
