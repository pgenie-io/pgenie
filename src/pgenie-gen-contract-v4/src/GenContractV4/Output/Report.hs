{-# LANGUAGE TemplateHaskell #-}

-- |
-- Generator diagnostics in v4 output.
module GenContractV4.Output.Report
  ( Report (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
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

Aeson.Deriver.derive [''Report]
