{-# LANGUAGE TemplateHaskell #-}

-- |
-- Enum alternatives used by custom enum types.
module GenContractV4.Contract.EnumVariant
  ( EnumVariant (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Contract.Name (Name)
import Utils.Prelude

-- | A variant in an enum type.
data EnumVariant = EnumVariant
  { name :: Name,
    pgName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary EnumVariant where
  arbitrary = EnumVariant <$> arbitrary <*> arbitrary

  shrink EnumVariant {name, pgName} =
    [ EnumVariant name' pgName'
    | (name', pgName') <- shrink (name, pgName)
    ]

Aeson.Deriver.derive [''EnumVariant]
