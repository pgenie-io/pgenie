-- |
-- Rendering of 'Report' as human-readable YAML, for logging a generator's
-- errors and warnings to the console.
module GenBridge.Model.Output.Report
  ( toErrorYamlText,
    toWarningYamlText,
  )
where

import GenBridge.Model.Output (Report (..))
import TextBuilder qualified
import Utils.Prelude

-- | Render a report under the given label (e.g. @"Error"@, @"Warning"@) as
-- a YAML list item, with its context path rendered as a nested list.
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
