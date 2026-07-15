{-# LANGUAGE TemplateHaskell #-}

-- |
-- Query template variables.
module GenContractV4.Input.Var
  ( Var (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input.Name (Name)
import Utils.Prelude

-- | A variable in a query fragment.
data Var = Var
  { name :: Name,
    rawName :: Text,
    paramIndex :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary Var where
  arbitrary = Var <$> arbitrary <*> arbitrary <*> arbitrary

  shrink Var {name, rawName, paramIndex} =
    [ Var name' rawName' paramIndex'
    | (name', rawName', paramIndex') <- shrink (name, rawName, paramIndex)
    ]

Aeson.Deriver.derive [''Var]
