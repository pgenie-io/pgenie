module Test.Report (toYamlText) where

import GenBridge.Contract qualified as Gen
import TextBuilder qualified
import Prelude

-- | Render a diagnostic report as YAML, labelling it with the given severity
-- (e.g. \"Error\", \"Warning\") for display in test output.
toYamlText :: Text -> Gen.Report -> Text
toYamlText label report =
  TextBuilder.toText
    . mconcat
    $ [ "- ",
        TextBuilder.text label,
        ": ",
        TextBuilder.text report.message,
        "\n  Context:",
        foldMap
          ( \p ->
              mconcat
                [ "\n    - ",
                  TextBuilder.text p
                ]
          )
          report.path
      ]
