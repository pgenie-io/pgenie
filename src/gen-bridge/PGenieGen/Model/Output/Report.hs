module PGenieGen.Model.Output.Report where

import PGenieGen.Model.Output (Report (..))
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

toErrorYamlText :: Report -> Text
toErrorYamlText = toYamlText "Error"

toWarningYamlText :: Report -> Text
toWarningYamlText = toYamlText "Warning"
