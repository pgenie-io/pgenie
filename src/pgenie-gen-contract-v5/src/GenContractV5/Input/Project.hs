{-# LANGUAGE TemplateHaskell #-}

-- |
-- Top-level v5 generator input projects.
module GenContractV5.Input.Project
  ( Project (..),
    toV4Project,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Input (Migration, Name, Version)
import GenContractV4.Input qualified as V4
import GenContractV5.Input.CustomType (CustomType, toV4CustomType)
import GenContractV5.Input.Query (Query, toV4Query)
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

-- | Downgrade a v5 project to the v4 input shape.
toV4Project :: Project -> V4.Project
toV4Project Project {space, name, version, customTypes, queries, migrations} =
  V4.Project
    { space,
      name,
      version,
      customTypes = map toV4CustomType customTypes,
      queries = map toV4Query queries,
      migrations
    }

Aeson.Deriver.derive [''Project]
