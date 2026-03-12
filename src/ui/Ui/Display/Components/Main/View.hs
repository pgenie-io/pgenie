module Ui.Display.Components.Main.View
  ( Scene (..),
    view,
  )
where

import Base.Prelude
import Data.Text qualified as Text
import TextBuilder
import TextBuilder qualified
import TextBuilderDev qualified

data Scene
  = StageEntered
      { clearBar :: Bool,
        path :: [Text],
        progress :: Double,
        timeLeft :: Maybe NominalDiffTime
      }
  | StageExited
      { path :: [Text],
        progress :: Double,
        timeLeft :: Maybe NominalDiffTime
      }
  | Warning
      { clearBar :: Bool,
        path :: [Text],
        message :: Text,
        suggestion :: Maybe Text,
        details :: [(Text, Text)],
        progress :: Double,
        timeLeft :: Maybe NominalDiffTime
      }
  | Error
      { clearBar :: Bool,
        path :: [Text],
        message :: Text,
        suggestion :: Maybe Text,
        details :: [(Text, Text)]
      }

view :: Scene -> TextBuilder
view = \case
  StageEntered {clearBar, path, progress, timeLeft} ->
    mconcat
      [ if clearBar then clearProgressBar else "",
        if null path then "" else TextBuilder.intercalateMap " > " to (reverse path) <> "\n",
        progressBar progress timeLeft
      ]
  StageExited {path, progress, timeLeft} ->
    mconcat
      [ moveCursorToLineStart,
        clearLine,
        if progress >= 1 || null path
          then green "Done!" <> "\n"
          else progressBar progress timeLeft
      ]
  Warning {clearBar, path, message, suggestion, details, progress, timeLeft} ->
    mconcat
      [ if clearBar then clearProgressBar else "",
        report (yellow "Warning") path message suggestion details,
        progressBar progress timeLeft
      ]
  Error {clearBar, path, message, suggestion, details} ->
    mconcat
      [ if clearBar then clearProgressBar else "",
        report (boldRed "Error") path message suggestion details
      ]

clearProgressBar :: TextBuilder
clearProgressBar = moveCursorToLineStart <> clearLine

clearLine :: TextBuilder
clearLine = "\ESC[2K"

moveCursorToLineStart :: TextBuilder
moveCursorToLineStart = "\r"

boldRed :: TextBuilder -> TextBuilder
boldRed text = "\ESC[1;31m" <> text <> "\ESC[0m"

green :: TextBuilder -> TextBuilder
green text = "\ESC[32m" <> text <> "\ESC[0m"

yellow :: TextBuilder -> TextBuilder
yellow text = "\ESC[33m" <> text <> "\ESC[0m"

progressBar :: Double -> Maybe NominalDiffTime -> TextBuilder
progressBar progress _timeLeftEstimate =
  let width = 30
      filledWidth = round (progress * fromIntegral width)
      emptyWidth = width - filledWidth
      filled = to (Text.replicate filledWidth "=")
      arrow = if filledWidth > 0 && filledWidth < width then ">" else ""
      empty = to (Text.replicate (emptyWidth - Text.length arrow) ".")
      percentage = TextBuilderDev.doubleFixedPointPercent 1 progress
   in mconcat ["[", filled, to arrow, empty, "] ", percentage]

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
