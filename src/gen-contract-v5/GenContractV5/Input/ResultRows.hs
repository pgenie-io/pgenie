{-# LANGUAGE TemplateHaskell #-}

-- |
-- Row-returning query payloads in the v5 contract model.
module GenContractV5.Input.ResultRows
  ( ResultRows (..),
    toV4ResultRows,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input qualified as V4
import GenContractV4.Input.ResultRowsCardinality (ResultRowsCardinality)
import GenContractV5.Input.Member (Member, toV4Member)
import Utils.Prelude

-- | Result rows with cardinality and row structure.
data ResultRows = ResultRows
  { cardinality :: ResultRowsCardinality,
    columns :: NonEmpty Member
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary ResultRows where
  arbitrary = ResultRows <$> arbitrary <*> arbitrary

  shrink ResultRows {cardinality, columns} =
    [ ResultRows cardinality' columns'
    | (cardinality', columns') <- shrink (cardinality, columns)
    ]

-- | Downgrade a v5 row result payload to the v4 shape.
toV4ResultRows :: ResultRows -> V4.ResultRows
toV4ResultRows ResultRows {cardinality, columns} =
  V4.ResultRows {cardinality, columns = fmap toV4Member columns}

Aeson.Deriver.derive [''ResultRows]
