{-# LANGUAGE TemplateHaskell #-}

-- |
-- Cardinality classification for row-returning queries.
module GenContractV4.Contract.ResultRowsCardinality
  ( ResultRowsCardinality (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import Test.QuickCheck qualified as QuickCheck
import Utils.Prelude

-- | Category of result rows.
data ResultRowsCardinality
  = OptionalResultRowsCardinality
  | SingleResultRowsCardinality
  | MultipleResultRowsCardinality
  deriving stock (Show, Eq, Generic, Bounded, Enum)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "ResultRowsCardinality") ResultRowsCardinality)

instance Arbitrary ResultRowsCardinality where
  arbitrary = QuickCheck.elements [minBound .. maxBound]

Aeson.Deriver.derive [''ResultRowsCardinality]
