{-# LANGUAGE TemplateHaskell #-}

-- |
-- Generator diagnostics in v4 output.
module GenContractV4.Output.Report
  ( Report (..),
    toErrorYamlText,
    toWarningYamlText,
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Orphans ()
import TextBuilder qualified
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

toYamlText :: Text -> Report -> Text
toYamlText label report =
  (from @TextBuilder.TextBuilder . mconcat)
    [ "- ",
      to label,
      ": ",
      to report.message,
      "\n  Context:",
      foldMap
        ( \p ->
            mconcat
              [ "\n    - ",
                to p
              ]
        )
        report.path
    ]

-- | Render a fatal generation-failure report as YAML.
toErrorYamlText :: Report -> Text
toErrorYamlText = toYamlText "Error"

-- | Render a non-fatal generation warning as YAML.
toWarningYamlText :: Report -> Text
toWarningYamlText = toYamlText "Warning"
