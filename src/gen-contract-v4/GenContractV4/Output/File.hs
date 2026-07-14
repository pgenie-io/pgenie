{-# LANGUAGE TemplateHaskell #-}

-- |
-- Generated files in contract generator output.
module GenContractV4.Output.File
  ( File (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
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

Aeson.Deriver.derive [''File]
