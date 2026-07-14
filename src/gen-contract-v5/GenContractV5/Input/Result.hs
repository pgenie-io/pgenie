{-# LANGUAGE TemplateHaskell #-}

-- |
-- Query result classifications in the v5 contract model.
module GenContractV5.Input.Result
  ( Result (..),
    toV4Result,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input qualified as V4
import GenContractV5.Input.ResultRows (ResultRows, toV4ResultRows)
import Test.QuickCheck qualified as QuickCheck
import Utils.Prelude

-- | Query result classification.
data Result
  = VoidResult
  | RowsAffectedResult
  | RowsResult ResultRows
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Result") Result)

instance Arbitrary Result where
  arbitrary =
    QuickCheck.oneof
      [ pure VoidResult,
        pure RowsAffectedResult,
        RowsResult <$> arbitrary
      ]

  shrink = \case
    VoidResult -> []
    RowsAffectedResult -> []
    RowsResult rows -> RowsResult <$> shrink rows

-- | Downgrade a v5 query result to the v4 shape.
toV4Result :: Result -> V4.Result
toV4Result = \case
  VoidResult -> V4.VoidResult
  RowsAffectedResult -> V4.RowsAffectedResult
  RowsResult rows -> V4.RowsResult (toV4ResultRows rows)

Aeson.Deriver.derive [''Result]
