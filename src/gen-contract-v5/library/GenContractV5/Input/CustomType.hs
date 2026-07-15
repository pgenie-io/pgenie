{-# LANGUAGE TemplateHaskell #-}

-- |
-- Named custom types in the v5 contract model.
module GenContractV5.Input.CustomType
  ( CustomType (..),
    toV4CustomType,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input (Name)
import GenContractV4.Input qualified as V4
import GenContractV5.Input.CustomTypeDefinition (CustomTypeDefinition, toV4CustomTypeDefinition)
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

-- | Downgrade a v5 custom type to the v4 shape.
toV4CustomType :: CustomType -> V4.CustomType
toV4CustomType CustomType {name, pgSchema, pgName, definition} =
  V4.CustomType
    { name,
      pgSchema,
      pgName,
      definition = toV4CustomTypeDefinition definition
    }

Aeson.Deriver.derive [''CustomType]
