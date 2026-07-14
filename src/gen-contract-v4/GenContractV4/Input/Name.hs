-- |
-- A precomputed-case identifier shared across the contract model.
module GenContractV4.Input.Name
  ( Name (..),
  )
where

import Data.Aeson qualified as Aeson
import Dhall qualified
import GenContractBase.Dhall.Orphans ()
import Test.QuickCheck qualified as QuickCheck
import Utils.Prelude

-- | A name with precomputed case renderings.
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

instance Arbitrary Name where
  arbitrary =
    Name
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

  shrink Name {inCamelCase, inPascalCase, inKebabCase, inTrainCase, inScreamingKebabCase, inSnakeCase, inCamelSnakeCase, inScreamingSnakeCase} =
    [ Name inCamelCase' inPascalCase' inKebabCase' inTrainCase' inScreamingKebabCase' inSnakeCase' inCamelSnakeCase' inScreamingSnakeCase'
    | ( inCamelCase',
        inPascalCase',
        inKebabCase',
        inTrainCase',
        inScreamingKebabCase',
        inSnakeCase',
        inCamelSnakeCase',
        inScreamingSnakeCase'
        ) <-
        QuickCheck.shrink
          ( inCamelCase,
            inPascalCase,
            inKebabCase,
            inTrainCase,
            inScreamingKebabCase,
            inSnakeCase,
            inCamelSnakeCase,
            inScreamingSnakeCase
          )
    ]

-- | Hand-written to preserve the pinned kebab-case JSON field names.
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
