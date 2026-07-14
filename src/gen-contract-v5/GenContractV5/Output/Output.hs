{-# LANGUAGE TemplateHaskell #-}

-- |
-- Top-level generator output results in v5.
module GenContractV5.Output.Output
  ( Output (..),
    toV5Output,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Output qualified as V4
import GenContractV5.Output.OutputOk (OutputOk, toV5OutputOk)
import GenContractV5.Output.Report (Report, toV5Report)
import Test.QuickCheck qualified as QuickCheck
import Utils.Prelude

-- | Either successful output or a fatal report.
data Output
  = OkOutput OutputOk
  | ErrOutput Report
  deriving stock (Generic, Show, Eq)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Output") Output)

instance Arbitrary Output where
  arbitrary =
    QuickCheck.oneof
      [ OkOutput <$> arbitrary,
        ErrOutput <$> arbitrary
      ]

  shrink = \case
    OkOutput outputOk -> OkOutput <$> shrink outputOk
    ErrOutput report -> ErrOutput <$> shrink report

-- | Upgrade a v4 output to the v5 shape.
toV5Output :: V4.Output -> Output
toV5Output = \case
  V4.OkOutput outputOk -> OkOutput (toV5OutputOk outputOk)
  V4.ErrOutput report -> ErrOutput (toV5Report report)

Aeson.Deriver.derive [''Output]
