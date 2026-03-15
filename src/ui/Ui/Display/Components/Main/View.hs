module Ui.Display.Components.Main.View
  ( eraseLine,
    printStagePath,
    printDone,
    printWarning,
    printError,
  )
where

import Data.Text qualified as Text
import TextBuilder
import TextBuilder qualified
import Utils.Prelude

-- | Erase the current terminal line in place (for overwriting a progress bar).
eraseLine :: TextBuilder
eraseLine = "\r\ESC[2K"

-- | Print the stage path breadcrumb.
printStagePath :: [Text] -> TextBuilder
printStagePath path =
  if null path
    then mempty
    else TextBuilder.intercalateMap " > " to (reverse path) <> "\n"

-- | Print the "Done!" completion message, replacing the current line.
printDone :: TextBuilder
printDone = "\r\ESC[2K" <> "\ESC[32mDone!\ESC[0m" <> "\n"

-- | Print a warning report.
printWarning :: [Text] -> Text -> Maybe Text -> [(Text, Text)] -> TextBuilder
printWarning path message suggestion details =
  report ("\ESC[33mWarning\ESC[0m") path message suggestion details

-- | Print an error report.
printError :: [Text] -> Text -> Maybe Text -> [(Text, Text)] -> TextBuilder
printError path message suggestion details =
  report ("\ESC[1;31mError\ESC[0m") path message suggestion details

report ::
  TextBuilder ->
  [Text] ->
  Text ->
  Maybe Text ->
  [(Text, Text)] ->
  TextBuilder
report label path message suggestion details =
  mconcat
    [ label,
      ": ",
      to message,
      "\n",
      if null path
        then ""
        else mconcat ["Stage: ", intercalateMap " > " to (reverse path), "\n"],
      maybe "" (mappend "Suggestion: " . flip mappend "\n" . to) suggestion,
      if null details
        then ""
        else
          mconcat
            [ "Details:\n",
              intercalateMap
                "\n"
                ( \(key, value) ->
                    if Text.any (== '\n') value
                      then "  " <> to key <> ":\n" <> indent 4 value
                      else "  " <> to key <> ": " <> to value
                )
                details,
              "\n"
            ],
      "\n"
    ]

indent :: Int -> Text -> TextBuilder
indent level text =
  let prefix = Text.replicate level " "
   in prefixEachLine (to prefix) text

prefixEachLine :: TextBuilder -> Text -> TextBuilder
prefixEachLine prefix text =
  intercalateMap "\n" (mappend prefix . to) (Text.lines text)
