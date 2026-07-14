{-# LANGUAGE TemplateHaskell #-}

-- |
-- Successful generator outputs in v5.
module GenContractV5.Output.OutputOk
  ( OutputOk (..),
    toV5OutputOk,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Output qualified as V4
import GenContractV5.Output.File (File, toV5File)
import GenContractV5.Output.Report (Report, toV5Report)
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

-- | Upgrade a v4 successful output payload to the v5 shape.
toV5OutputOk :: V4.OutputOk -> OutputOk
toV5OutputOk V4.OutputOk {warnings, value} =
  OutputOk {warnings = map toV5Report warnings, value = map toV5File value}

Aeson.Deriver.derive [''OutputOk]
