{-# LANGUAGE TemplateHaskell #-}

-- |
-- Named custom types in the v4 contract model.
module GenContractV4.Input.CustomType
  ( CustomType (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input.CustomTypeDefinition (CustomTypeDefinition)
import GenContractV4.Input.Name (Name)
import Utils.Prelude

-- | A custom type with its project identifier, PostgreSQL identity, and definition.
data CustomType = CustomType
  { name :: Name,
    pgSchema :: Text,
    pgName :: Text,
    definition :: CustomTypeDefinition
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary CustomType where
  arbitrary = CustomType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  shrink CustomType {name, pgSchema, pgName, definition} =
    [ CustomType name' pgSchema' pgName' definition'
    | (name', pgSchema', pgName', definition') <- shrink (name, pgSchema, pgName, definition)
    ]

Aeson.Deriver.derive [''CustomType]
