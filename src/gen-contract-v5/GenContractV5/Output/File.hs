{-# LANGUAGE TemplateHaskell #-}

-- |
-- Generated files in the v5 output model.
module GenContractV5.Output.File
  ( File (..),
    toV5File,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Output qualified as V4
import Utils.Prelude

-- | A single generated file, relative to the generation output directory.
data File = File
  { path :: Text,
    content :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

instance Arbitrary File where
  arbitrary = File <$> arbitrary <*> arbitrary

  shrink File {path, content} =
    [ File path' content'
    | (path', content') <- shrink (path, content)
    ]

-- | Upgrade a v4 file to the v5 output shape.
toV5File :: V4.File -> File
toV5File V4.File {path, content} = File {path, content}

Aeson.Deriver.derive [''File]
