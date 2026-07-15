-- |
-- Terminal rendering primitives for the main display component: breadcrumbs,
-- the completion message, and warning/error reports.
module Ui.Display.Components.Main.View
  ( eraseLine,
    printStagePath,
    printStageDone,
    printDone,
    printWarning,
    printError,
  )
where

import Data.Text qualified as Text
import TextBuilder
import Utils.Prelude

-- | Erase the current terminal line in place (for overwriting a progress bar).
eraseLine :: TextBuilder
eraseLine = "\r\ESC[2K"

-- | Render the breadcrumb of nested stage names leading to the current one.
printStagePath :: [Text] -> TextBuilder
printStagePath path =
  if null path
    then mempty
    else TextBuilder.intercalateMap " > " to (reverse path) <> "\n"

-- | Print the stage path breadcrumb with a trailing green "Done" status.
printStageDone :: [Text] -> TextBuilder
printStageDone path =
  case path of
    [] -> printDone
    _ ->
      TextBuilder.intercalateMap " > " to (reverse path) <> " > \ESC[1;32mDone\ESC[0m\n"

-- | Print the "Done!" completion message, replacing the current line.
printDone :: TextBuilder
printDone = "\r\ESC[2K" <> "\ESC[1;32mDone!\ESC[0m" <> "\n"

-- | Render a warning report: message, optional stage path, suggestion, and
-- key/value details, colour-coded yellow.
printWarning :: [Text] -> Text -> Maybe Text -> [(Text, Text)] -> TextBuilder
printWarning path message suggestion details =
  report "1;33" "Warning" path message suggestion details

-- | Render an error report: message, optional stage path, suggestion, and
-- key/value details, colour-coded red.
printError :: [Text] -> Text -> Maybe Text -> [(Text, Text)] -> TextBuilder
printError path message suggestion details =
  report "1;31" "Error" path message suggestion details

report ::
  TextBuilder ->
  TextBuilder ->
  [Text] ->
  Text ->
  Maybe Text ->
  [(Text, Text)] ->
  TextBuilder
report modifiers label path message suggestion details =
  mconcat
    [ "\ESC[",
      modifiers,
      "m",
      label,
      ": ",
      to message,
      "\ESC[0m",
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
  where
    indent :: Int -> Text -> TextBuilder
    indent level text =
      let prefix = Text.replicate level " "
       in prefixEachLine (to prefix) text
      where
        prefixEachLine :: TextBuilder -> Text -> TextBuilder
        prefixEachLine prefix' text' =
          intercalateMap "\n" (mappend prefix' . to) (Text.lines text')
