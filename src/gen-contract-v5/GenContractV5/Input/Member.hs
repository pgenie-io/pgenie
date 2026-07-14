{-# LANGUAGE TemplateHaskell #-}

-- |
-- Fields in composite types, query params, and result rows.
module GenContractV5.Input.Member
  ( Member (..),
    toV4Member,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input qualified as V4
import GenContractV4.Input.Name (Name)
import GenContractV5.Input.Value (Value, toV4Value)
import Utils.Prelude

-- | A field in a composite type or query parameter.
data Member = Member
  { name :: Name,
    pgName :: Text,
    isNullable :: Bool,
    value :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary Member where
  arbitrary = Member <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  shrink Member {name, pgName, isNullable, value} =
    [ Member name' pgName' isNullable' value'
    | (name', pgName', isNullable', value') <- shrink (name, pgName, isNullable, value)
    ]

-- | Downgrade a v5 member to the v4 member shape.
toV4Member :: Member -> V4.Member
toV4Member Member {name, pgName, isNullable, value} =
  V4.Member {name, pgName, isNullable, value = toV4Value value}

Aeson.Deriver.derive [''Member]
