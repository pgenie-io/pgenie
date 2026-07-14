{-# LANGUAGE TemplateHaskell #-}

-- |
-- Definitions of custom contract types.
module GenContractV5.Input.CustomTypeDefinition
  ( CustomTypeDefinition (..),
    toV4CustomTypeDefinition,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input qualified as V4
import GenContractV5.Input.EnumVariant (EnumVariant)
import GenContractV5.Input.Member (Member, toV4Member)
import GenContractV5.Input.Value (Value, toV4Value)
import Test.QuickCheck qualified as QuickCheck
import Utils.Prelude

-- | Definition of a custom type.
data CustomTypeDefinition
  = CompositeCustomTypeDefinition [Member]
  | EnumCustomTypeDefinition [EnumVariant]
  | DomainCustomTypeDefinition Value
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "CustomTypeDefinition") CustomTypeDefinition)

instance Arbitrary CustomTypeDefinition where
  arbitrary =
    QuickCheck.oneof
      [ CompositeCustomTypeDefinition <$> arbitrary,
        EnumCustomTypeDefinition <$> arbitrary,
        DomainCustomTypeDefinition <$> arbitrary
      ]

  shrink = \case
    CompositeCustomTypeDefinition members -> CompositeCustomTypeDefinition <$> shrink members
    EnumCustomTypeDefinition variants -> EnumCustomTypeDefinition <$> shrink variants
    DomainCustomTypeDefinition value -> DomainCustomTypeDefinition <$> shrink value

-- | Downgrade a v5 custom type definition to the v4 shape.
toV4CustomTypeDefinition :: CustomTypeDefinition -> V4.CustomTypeDefinition
toV4CustomTypeDefinition = \case
  CompositeCustomTypeDefinition members ->
    V4.CompositeCustomTypeDefinition (map toV4Member members)
  EnumCustomTypeDefinition variants ->
    V4.EnumCustomTypeDefinition variants
  DomainCustomTypeDefinition value ->
    V4.DomainCustomTypeDefinition (toV4Value value)

Aeson.Deriver.derive [''CustomTypeDefinition]
