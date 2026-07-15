{-# LANGUAGE TemplateHaskell #-}

-- |
-- Top-level v4 generator input projects.
module GenContractV4.Input.Project
  ( Project (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input.CustomType (CustomType)
import GenContractV4.Input.Migration (Migration)
import GenContractV4.Input.Name (Name)
import GenContractV4.Input.Query (Query)
import GenContractV4.Input.Version (Version)
import Utils.Prelude hiding (Version)

-- | A project with name, version, custom types, queries, and migrations.
data Project = Project
  { space :: Name,
    name :: Name,
    version :: Version,
    customTypes :: [CustomType],
    queries :: [Query],
    migrations :: [Migration]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

instance Arbitrary Project where
  arbitrary = Project <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  shrink Project {space, name, version, customTypes, queries, migrations} =
    [ Project space' name' version' customTypes' queries' migrations'
    | (space', name', version', customTypes', queries', migrations') <-
        shrink (space, name, version, customTypes, queries, migrations)
    ]

Aeson.Deriver.derive [''Project]
