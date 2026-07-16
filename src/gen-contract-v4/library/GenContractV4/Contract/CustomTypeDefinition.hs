{-# LANGUAGE TemplateHaskell #-}

-- |
-- Definitions of custom contract types.
module GenContractV4.Contract.CustomTypeDefinition
  ( CustomTypeDefinition (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Contract.EnumVariant (EnumVariant)
import GenContractV4.Contract.Member (Member)
import GenContractV4.Contract.Value (Value)
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

Aeson.Deriver.derive [''CustomTypeDefinition]
