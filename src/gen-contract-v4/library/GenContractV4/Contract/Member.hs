{-# LANGUAGE TemplateHaskell #-}

-- |
-- Fields in composite types, query params, and result rows.
module GenContractV4.Contract.Member
  ( Member (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Contract.Name (Name)
import GenContractV4.Contract.Value (Value)
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

Aeson.Deriver.derive [''Member]
