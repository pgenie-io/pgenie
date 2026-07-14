{-# LANGUAGE TemplateHaskell #-}

-- |
-- SQL or variable fragments that make up a query template.
module GenContractV4.Input.QueryFragment
  ( QueryFragment (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input.Var (Var)
import Test.QuickCheck qualified as QuickCheck
import Utils.Prelude

-- | A fragment of a query, either SQL text or a variable.
data QueryFragment
  = SqlQueryFragment Text
  | VarQueryFragment Var
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "QueryFragment") QueryFragment)

instance Arbitrary QueryFragment where
  arbitrary =
    QuickCheck.oneof
      [ SqlQueryFragment <$> arbitrary,
        VarQueryFragment <$> arbitrary
      ]

  shrink = \case
    SqlQueryFragment sql -> SqlQueryFragment <$> shrink sql
    VarQueryFragment var -> VarQueryFragment <$> shrink var

Aeson.Deriver.derive [''QueryFragment]
