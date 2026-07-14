{-# LANGUAGE TemplateHaskell #-}

-- |
-- Resolvable references to custom types in the v5 contract model.
module GenContractV5.Input.CustomTypeRef
  ( CustomTypeRef (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV5.Input.Name (Name)
import Utils.Prelude

-- | A custom type's identifier plus its PostgreSQL identity and index.
data CustomTypeRef = CustomTypeRef
  { name :: Name,
    pgSchema :: Text,
    pgName :: Text,
    index :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary CustomTypeRef where
  arbitrary = CustomTypeRef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  shrink CustomTypeRef {name, pgSchema, pgName, index} =
    [ CustomTypeRef name' pgSchema' pgName' index'
    | (name', pgSchema', pgName', index') <- shrink (name, pgSchema, pgName, index)
    ]

Aeson.Deriver.derive [''CustomTypeRef]
