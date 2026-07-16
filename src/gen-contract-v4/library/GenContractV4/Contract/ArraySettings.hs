{-# LANGUAGE TemplateHaskell #-}

-- |
-- Array metadata attached to a value in the v4 contract model.
module GenContractV4.Contract.ArraySettings
  ( ArraySettings (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import Utils.Prelude

-- | An array-wrapping annotation for a value, present only when
-- @dimensionality > 0@.
data ArraySettings = ArraySettings
  { dimensionality :: Natural,
    elementIsNullable :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary ArraySettings where
  arbitrary = ArraySettings <$> arbitrary <*> arbitrary

  shrink ArraySettings {dimensionality, elementIsNullable} =
    [ ArraySettings dimensionality' elementIsNullable'
    | (dimensionality', elementIsNullable') <- shrink (dimensionality, elementIsNullable)
    ]

Aeson.Deriver.derive [''ArraySettings]
