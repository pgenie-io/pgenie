{-# LANGUAGE TemplateHaskell #-}

-- |
-- Member and domain value shapes in the v5 contract model.
module GenContractV5.Input.Value
  ( Value (..),
    toV4Value,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input qualified as V4
import GenContractV5.Input.Scalar (Scalar, toV4Scalar)
import Utils.Prelude

-- | A column or parameter type with inlined array metadata.
data Value = Value
  { dimensionality :: Natural,
    elementIsNullable :: Bool,
    scalar :: Scalar
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary Value where
  arbitrary = Value <$> arbitrary <*> arbitrary <*> arbitrary

  shrink Value {dimensionality, elementIsNullable, scalar} =
    [ Value dimensionality' elementIsNullable' scalar'
    | (dimensionality', elementIsNullable', scalar') <- shrink (dimensionality, elementIsNullable, scalar)
    ]

-- | Downgrade a v5 value to the v4 value shape.
toV4Value :: Value -> V4.Value
toV4Value Value {dimensionality, elementIsNullable, scalar} =
  V4.Value
    { arraySettings =
        if dimensionality == 0
          then Nothing
          else Just V4.ArraySettings {dimensionality, elementIsNullable},
      scalar = toV4Scalar scalar
    }

Aeson.Deriver.derive [''Value]
