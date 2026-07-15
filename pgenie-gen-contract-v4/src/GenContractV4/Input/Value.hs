{-# LANGUAGE TemplateHaskell #-}

-- |
-- Member and domain value shapes in the v4 contract model.
module GenContractV4.Input.Value
  ( Value (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input.ArraySettings (ArraySettings)
import GenContractV4.Input.Scalar (Scalar)
import Utils.Prelude

-- | A column or parameter type: a scalar, optionally array-wrapped.
data Value = Value
  { arraySettings :: Maybe ArraySettings,
    scalar :: Scalar
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary Value where
  arbitrary = Value <$> arbitrary <*> arbitrary

  shrink Value {arraySettings, scalar} =
    [ Value arraySettings' scalar'
    | (arraySettings', scalar') <- shrink (arraySettings, scalar)
    ]

Aeson.Deriver.derive [''Value]
