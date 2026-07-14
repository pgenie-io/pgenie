{-# LANGUAGE TemplateHaskell #-}

-- |
-- Query result classifications in the v4 contract model.
module GenContractV4.Input.Result
  ( Result (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input.ResultRows (ResultRows)
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

Aeson.Deriver.derive [''Result]
