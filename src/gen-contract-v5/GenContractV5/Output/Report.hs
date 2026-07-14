{-# LANGUAGE TemplateHaskell #-}

-- |
-- Generator diagnostics in v5 output.
module GenContractV5.Output.Report
  ( Report (..),
    toV5Report,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import GenContractV4.Output qualified as V4
import Utils.Prelude

-- | Diagnostic report with a context path and a message.
data Report = Report
  { path :: [Text],
    message :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

instance Arbitrary Report where
  arbitrary = Report <$> arbitrary <*> arbitrary

  shrink Report {path, message} =
    [ Report path' message'
    | (path', message') <- shrink (path, message)
    ]

-- | Upgrade a v4 report to the v5 output shape.
toV5Report :: V4.Report -> Report
toV5Report V4.Report {path, message} = Report {path, message}

Aeson.Deriver.derive [''Report]
