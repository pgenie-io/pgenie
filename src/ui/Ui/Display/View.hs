module Ui.Display.View
  ( view,
  )
where

import Base.Prelude
import Data.Text qualified as Text
import Logic.Algebra qualified as Logic
import TextBuilder
import TextBuilder qualified
import TextBuilderDev qualified
import Ui.Display.Memory (Memory (..))

-- | View function: event -> oldModel -> newModel -> TextBuilder
-- Renders the state transition following Elm architecture
view :: Logic.Event -> Memory -> Memory -> TextBuilder
view event oldMemory newMemory = case event of
  Logic.StageEntered path ->
    if oldMemory.progress >= 1
      then mempty -- Don't update if already at 100%
      else
        mconcat
          [ if oldMemory.hasProgressBar
              then clearProgressBar
              else "",
            if null path
              then ""
              else TextBuilder.intercalateMap " > " to (reverse path) <> "\n",
            progressBar newMemory.progress newMemory.timeLeftEstimate
          ]
  Logic.StageExited _path _progressDelta ->
    if oldMemory.progress >= 1
      then mempty -- Don't update progress if already at 100%
      else
        if oldMemory.hasProgressBar
          then
            mconcat
              [ moveCursorToLineStart,
                clearLine,
                if newMemory.progress < 1
                  then progressBar newMemory.progress newMemory.timeLeftEstimate
                  else green "Done!" <> "\n"
              ]
          else mempty -- Don't render progress bar if it's not shown
  Logic.WarningEmitted err ->
    mconcat
      [ if oldMemory.hasProgressBar
          then clearProgressBar
          else "",
        report (yellow "Warning") err.path err.message err.suggestion err.details,
        if newMemory.hasProgressBar
          then progressBar newMemory.progress newMemory.timeLeftEstimate
          else ""
      ]
  Logic.Failed err ->
    mconcat
      [ if oldMemory.hasProgressBar
          then clearProgressBar
          else "",
        report (boldRed "Error") err.path err.message err.suggestion err.details,
        if newMemory.hasProgressBar
          then progressBar newMemory.progress newMemory.timeLeftEstimate
          else ""
      ]

clearProgressBar :: TextBuilder
clearProgressBar = moveCursorToLineStart <> clearLine

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
          "] ",
          percentage
        ]

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
        else
          mconcat
            [ "Stage: ",
              intercalateMap " > " to (reverse path),
              "\n"
            ],
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
                      then
                        "  " <> to key <> ":\n" <> indent 4 value
                      else
                        "  " <> to key <> ": " <> to value
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
