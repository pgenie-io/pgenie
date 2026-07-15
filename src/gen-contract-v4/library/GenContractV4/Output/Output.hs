{-# LANGUAGE TemplateHaskell #-}

-- |
-- Top-level generator output results in v4.
module GenContractV4.Output.Output
  ( Output (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Output.OutputOk (OutputOk)
import GenContractV4.Output.Report (Report)
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

Aeson.Deriver.derive [''Output]
