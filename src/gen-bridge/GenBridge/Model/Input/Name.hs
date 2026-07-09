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

-- | Hand-written, not run through 'GenBridge.Aeson.Deriver.derive': fields
-- are kebab-cased to match the rest of the 'Input' model JSON contract.
instance Aeson.ToJSON Name where
  toJSON (Name cc pc kc tc skc sc csc ssc) =
    Aeson.object
      [ "in-camel-case" Aeson..= cc,
        "in-pascal-case" Aeson..= pc,
        "in-kebab-case" Aeson..= kc,
        "in-train-case" Aeson..= tc,
        "in-screaming-kebab-case" Aeson..= skc,
        "in-snake-case" Aeson..= sc,
        "in-camel-snake-case" Aeson..= csc,
        "in-screaming-snake-case" Aeson..= ssc
      ]

instance Aeson.FromJSON Name where
  parseJSON = Aeson.withObject "Name" \obj ->
    Name
      <$> obj
      Aeson..: "in-camel-case"
      <*> obj
      Aeson..: "in-pascal-case"
      <*> obj
      Aeson..: "in-kebab-case"
      <*> obj
      Aeson..: "in-train-case"
      <*> obj
      Aeson..: "in-screaming-kebab-case"
      <*> obj
      Aeson..: "in-snake-case"
      <*> obj
      Aeson..: "in-camel-snake-case"
      <*> obj
      Aeson..: "in-screaming-snake-case"
