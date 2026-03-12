module Ui.Display.Components.Main
  ( Memory,
    init,
    update,
  )
where

import Base.Prelude hiding (init)
import Data.Text qualified as Text
import Logic.Algebra qualified as Logic
import TextBuilder
import TextBuilder qualified
import TextBuilderDev qualified

-- | Display state
data Memory = Memory
  { -- | Overall progress.
    progress :: Double,
    -- | Whether we've printed progress bar yet
    hasProgressBar :: Bool,
    startTime :: UTCTime,
    timeLeftEstimate :: Maybe NominalDiffTime
  }
  deriving stock (Eq, Show)

init :: UTCTime -> Memory
init startTime =
  Memory
    { progress = 0,
      hasProgressBar = False,
      startTime,
      timeLeftEstimate = Nothing
    }

-- | Combined update: transitions state and produces terminal output
update :: Logic.Event -> UTCTime -> Memory -> (Memory, TextBuilder)
update event currentTime old =
  let new = advance event old & setCurrentTime currentTime
   in (new, render event old new)

-- State transition

advance :: Logic.Event -> Memory -> Memory
advance event memory =
  case event of
    Logic.StageEntered _path ->
      memory {hasProgressBar = True}
    Logic.StageExited _path progressDelta ->
      if memory.progress >= 1
        then memory
        else
          let newProgress = memory.progress + progressDelta
              newProgressCapped = if newProgress >= 0.999 then 1 else newProgress
           in memory {progress = newProgressCapped}
    Logic.WarningEmitted _err ->
      memory {hasProgressBar = True}
    Logic.Failed _err ->
      memory {hasProgressBar = False}

setCurrentTime :: UTCTime -> Memory -> Memory
setCurrentTime currentTime memory =
  let elapsedTime = currentTime `diffUTCTime` memory.startTime
      newTimeLeftEstimate =
        if memory.progress > 0
          then Just (elapsedTime / realToFrac memory.progress - elapsedTime)
          else Nothing
   in memory {timeLeftEstimate = newTimeLeftEstimate}

-- Rendering

render :: Logic.Event -> Memory -> Memory -> TextBuilder
render event old new = case event of
  Logic.StageEntered path ->
    if old.progress >= 1
      then mempty
      else
        mconcat
          [ if old.hasProgressBar then clearProgressBar else "",
            if null path then "" else TextBuilder.intercalateMap " > " to (reverse path) <> "\n",
            progressBar new.progress new.timeLeftEstimate
          ]
  Logic.StageExited path _progressDelta ->
    if old.progress < 1 && old.hasProgressBar
      then
        mconcat
          [ moveCursorToLineStart,
            clearLine,
            if new.progress >= 1 || null path
              then green "Done!" <> "\n"
              else progressBar new.progress new.timeLeftEstimate
          ]
      else mempty
  Logic.WarningEmitted err ->
    mconcat
      [ if old.hasProgressBar then clearProgressBar else "",
        report (yellow "Warning") err.path err.message err.suggestion err.details,
        if new.hasProgressBar then progressBar new.progress new.timeLeftEstimate else ""
      ]
  Logic.Failed err ->
    mconcat
      [ if old.hasProgressBar then clearProgressBar else "",
        report (boldRed "Error") err.path err.message err.suggestion err.details,
        if new.hasProgressBar then progressBar new.progress new.timeLeftEstimate else ""
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
progressBar progress timeLeftEstimate =
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
