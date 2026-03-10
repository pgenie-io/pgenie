module PGenieGen.Model.Output.Report where

import Dhall qualified
import PGenieGen.Prelude
import TextBuilder qualified

data Report = Report
  { path :: [Text],
    message :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

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
