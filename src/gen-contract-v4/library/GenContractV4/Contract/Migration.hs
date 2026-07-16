{-# LANGUAGE TemplateHaskell #-}

-- |
-- SQL migrations embedded in a contract project.
module GenContractV4.Contract.Migration
  ( Migration (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import Utils.Prelude

-- | A single SQL migration file, applied in the order it appears in a project.
data Migration = Migration
  { name :: Text,
    sql :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary Migration where
  arbitrary = Migration <$> arbitrary <*> arbitrary

  shrink Migration {name, sql} =
    [ Migration name' sql'
    | (name', sql') <- shrink (name, sql)
    ]

Aeson.Deriver.derive [''Migration]
