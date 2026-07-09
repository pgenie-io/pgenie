module GenBridge.Model.Input.Name where

import Data.Aeson qualified as Aeson
import Dhall qualified
import GenBridge.Dhall.Orphans ()
import GenBridge.Prelude

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

-- | Hand-written, not run through 'GenBridge.AesonDeriver.derive': fields
-- keep their literal camelCase Haskell names as JSON keys.
instance Aeson.ToJSON Name where
  toJSON (Name cc pc kc tc skc sc csc ssc) =
    Aeson.object
      [ "inCamelCase" Aeson..= cc,
        "inPascalCase" Aeson..= pc,
        "inKebabCase" Aeson..= kc,
        "inTrainCase" Aeson..= tc,
        "inScreamingKebabCase" Aeson..= skc,
        "inSnakeCase" Aeson..= sc,
        "inCamelSnakeCase" Aeson..= csc,
        "inScreamingSnakeCase" Aeson..= ssc
      ]

instance Aeson.FromJSON Name where
  parseJSON = Aeson.withObject "Name" \obj ->
    Name
      <$> obj
      Aeson..: "inCamelCase"
      <*> obj
      Aeson..: "inPascalCase"
      <*> obj
      Aeson..: "inKebabCase"
      <*> obj
      Aeson..: "inTrainCase"
      <*> obj
      Aeson..: "inScreamingKebabCase"
      <*> obj
      Aeson..: "inSnakeCase"
      <*> obj
      Aeson..: "inCamelSnakeCase"
      <*> obj
      Aeson..: "inScreamingSnakeCase"
