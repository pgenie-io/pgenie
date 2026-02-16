module Infra.Adapters.Display.View.TextBuilders where

import Base.Prelude
import Data.Text qualified as Text
import Logic qualified
import TextBuilder qualified
import TextBuilderDev qualified

-- | ANSI escape codes for terminal control
clearLine :: TextBuilder
clearLine = "\ESC[2K"

moveCursorToLineStart :: TextBuilder
moveCursorToLineStart = "\r"

-- | ANSI color codes
boldRed :: TextBuilder -> TextBuilder
boldRed text = "\ESC[1;31m" <> text <> "\ESC[0m"

green :: TextBuilder -> TextBuilder
green text = "\ESC[32m" <> text <> "\ESC[0m"

yellow :: TextBuilder -> TextBuilder
yellow text = "\ESC[33m" <> text <> "\ESC[0m"

-- | Render an ASCII progress bar
progressBar :: Double -> Maybe NominalDiffTime -> TextBuilder
progressBar progress timeLeftEstimate =
  let width =
        30
      filledWidth =
        round (progress * fromIntegral width)
      emptyWidth =
        width - filledWidth
      filled =
        to (Text.replicate filledWidth "=")
      arrow =
        if filledWidth > 0 && filledWidth < width then ">" else ""
      empty =
        to (Text.replicate (emptyWidth - Text.length arrow) ".")
      percentage =
        TextBuilderDev.doubleFixedPointPercent 1 progress
   in mconcat
        [ "[",
          filled,
          to arrow,
          empty,
          "]  ",
          percentage,
          case timeLeftEstimate of
            Nothing -> ""
            Just t ->
              " ("
                <> TextBuilderDev.doubleFixedPoint 1 (realToFrac t :: Double)
                <> "s left)"
        ]

report ::
  Bool ->
  TextBuilder ->
  [Text] ->
  Text ->
  Maybe Text ->
  [(Text, Text)] ->
  TextBuilder
report hasProgressBar label path message suggestion details =
  mconcat
    [ if hasProgressBar
        then moveCursorToLineStart <> clearLine -- Clear progress bar
        else "",
      "\n",
      label,
      ": ",
      to message,
      "\n",
      if null path
        then ""
        else
          mconcat
            [ "Stage: ",
              TextBuilder.intercalateMap " > " to (reverse path),
              "\n"
            ],
      maybe "" (mappend "Suggestion: " . flip mappend "\n" . to) suggestion,
      if null details
        then ""
        else
          mconcat
            [ "Details:\n",
              TextBuilder.intercalateMap
                "\n"
                ( \(key, value) ->
                    if Text.any (== '\n') value
                      then
                        "  " <> to key <> ":\n" <> indent 4 value
                      else
                        "  " <> to key <> ": " <> to value
                )
                details,
              "\n"
            ]
    ]

indent :: Int -> Text -> TextBuilder
indent level text =
  let prefix = Text.replicate level " "
   in prefixEachLine (to prefix) text

prefixEachLine :: TextBuilder -> Text -> TextBuilder
prefixEachLine prefix text =
  TextBuilder.intercalateMap "\n" (mappend prefix . to) (Text.lines text)
