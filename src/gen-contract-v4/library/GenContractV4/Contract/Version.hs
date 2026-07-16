{-# LANGUAGE TemplateHaskell #-}

-- |
-- Semantic version components for a contract project.
module GenContractV4.Contract.Version
  ( Version (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import Utils.Prelude hiding (Version)

-- | Version with semantic versioning components.
data Version = Version
  { major :: Natural,
    minor :: Natural,
    patch :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary Version where
  arbitrary = Version <$> arbitrary <*> arbitrary <*> arbitrary

  shrink Version {major, minor, patch} =
    [ Version major' minor' patch'
    | (major', minor', patch') <- shrink (major, minor, patch)
    ]

Aeson.Deriver.derive [''Version]
