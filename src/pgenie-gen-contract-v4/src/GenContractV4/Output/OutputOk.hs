{-# LANGUAGE TemplateHaskell #-}

-- |
-- Successful generator outputs in v4.
module GenContractV4.Output.OutputOk
  ( OutputOk (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Output.File (File)
import GenContractV4.Output.Report (Report)
import Utils.Prelude

-- | Successful generator output.
data OutputOk = OutputOk
  { warnings :: [Report],
    value :: [File]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

instance Arbitrary OutputOk where
  arbitrary = OutputOk <$> arbitrary <*> arbitrary

  shrink OutputOk {warnings, value} =
    [ OutputOk warnings' value'
    | (warnings', value') <- shrink (warnings, value)
    ]

Aeson.Deriver.derive [''OutputOk]
