-- |
-- Rendering of generator reports as human-readable YAML.
module GenContractV5.Output.ReportYaml
  ( toErrorYamlText,
    toWarningYamlText,
  )
where

import GenContractV5.Output.Report (Report (..))
import TextBuilder qualified
import Utils.Prelude

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
