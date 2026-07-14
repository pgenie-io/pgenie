{-# LANGUAGE TemplateHaskell #-}

-- |
-- Top-level generator output results in v5.
module GenContractV5.Output.Output
  ( Output (..),
    fromV4Output,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Output qualified as V4
import GenContractV4.Output.Report (Report)
import GenContractV5.Output.OutputOk (OutputOk, fromV4OutputOk)
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
fromV4Output :: V4.Output -> Output
fromV4Output = \case
  V4.OkOutput outputOk -> OkOutput (fromV4OutputOk outputOk)
  V4.ErrOutput report -> ErrOutput report

Aeson.Deriver.derive [''Output]
