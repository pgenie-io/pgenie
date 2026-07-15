{-# LANGUAGE TemplateHaskell #-}

-- |
-- Scalar value shapes in the v4 contract model.
module GenContractV4.Input.Scalar
  ( Scalar (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input.Name (Name)
import GenContractV4.Input.Primitive (Primitive)
import Test.QuickCheck qualified as QuickCheck
import Utils.Prelude

-- | Either a primitive type or a custom type name.
data Scalar
  = PrimitiveScalar Primitive
  | CustomScalar Name
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Scalar") Scalar)

instance Arbitrary Scalar where
  arbitrary =
    QuickCheck.oneof
      [ PrimitiveScalar <$> arbitrary,
        CustomScalar <$> arbitrary
      ]

  shrink = \case
    PrimitiveScalar primitive -> PrimitiveScalar <$> shrink primitive
    CustomScalar name -> CustomScalar <$> shrink name

Aeson.Deriver.derive [''Scalar]
