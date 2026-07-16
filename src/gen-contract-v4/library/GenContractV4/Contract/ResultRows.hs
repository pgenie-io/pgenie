{-# LANGUAGE TemplateHaskell #-}

-- |
-- Row-returning query payloads in the v4 contract model.
module GenContractV4.Contract.ResultRows
  ( ResultRows (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Contract.Member (Member)
import GenContractV4.Contract.ResultRowsCardinality (ResultRowsCardinality)
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

Aeson.Deriver.derive [''ResultRows]
