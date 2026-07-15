{-# LANGUAGE TemplateHaskell #-}

-- |
-- Scalar value shapes in the v5 contract model.
module GenContractV5.Input.Scalar
  ( Scalar (..),
    toV4Scalar,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input (Primitive)
import GenContractV4.Input qualified as V4
import GenContractV5.Input.CustomTypeRef (CustomTypeRef (..))
import Test.QuickCheck qualified as QuickCheck
import Utils.Prelude

-- | Either a primitive type or a resolvable custom type reference.
data Scalar
  = PrimitiveScalar Primitive
  | CustomScalar CustomTypeRef
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
    CustomScalar customTypeRef -> CustomScalar <$> shrink customTypeRef

-- | Downgrade a v5 scalar to the v4 scalar shape.
toV4Scalar :: Scalar -> V4.Scalar
toV4Scalar = \case
  PrimitiveScalar primitive -> V4.PrimitiveScalar primitive
  CustomScalar CustomTypeRef {name} -> V4.CustomScalar name

Aeson.Deriver.derive [''Scalar]
