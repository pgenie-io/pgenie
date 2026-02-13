module Infra.Adapters.Display.View
  ( view,
  )
where

import Base.Prelude
import Infra.Adapters.Display.Memory (Memory (..))
import Infra.Adapters.Display.View.TextBuilders qualified as TextBuilders
import Logic.Algebra qualified as Logic
import TextBuilder qualified

-- | View function: event -> oldModel -> newModel -> TextBuilder
-- Renders the state transition following Elm architecture
view :: Logic.Event -> Memory -> Memory -> TextBuilder
view event oldMemory newMemory = case event of
  Logic.StageEntered path ->
    mconcat
      [ if oldMemory.hasProgressBar
          then TextBuilders.moveCursorToLineStart <> TextBuilders.clearLine
          else "",
        if null path
          then ""
          else TextBuilder.intercalateMap " > " to (reverse path) <> "\n",
        TextBuilders.progressBar newMemory.progress newMemory.timeLeftEstimate
      ]
  Logic.StageExited _path _progressDelta ->
    if oldMemory.progress >= 1
      then mempty -- Don't update progress if already at 100%
      else
        mconcat
          [ TextBuilders.moveCursorToLineStart,
            TextBuilders.clearLine,
            if newMemory.progress < 1
              then TextBuilders.progressBar newMemory.progress newMemory.timeLeftEstimate
              else TextBuilders.green "Done!" <> "\n"
          ]
  Logic.WarningEmitted err ->
    mconcat
      [ if oldMemory.hasProgressBar
          then TextBuilders.moveCursorToLineStart <> TextBuilders.clearLine -- Clear progress bar
          else "",
        "\n",
        TextBuilders.yellow "Warning",
        ": ",
        to err.message,
        "\n",
        if null err.path
          then ""
          else
            "Location: "
              <> TextBuilder.intercalateMap " > " to (reverse err.path)
              <> "\n",
        maybe "" (mappend "Suggestion: " . to . mappend "\n") err.suggestion,
        if null err.details
          then ""
          else
            "Details:\n"
              <> TextBuilder.intercalateMap
                "\n"
                ( \(key, value) ->
                    "  " <> to key <> ": " <> to value
                )
                err.details
              <> "\n"
      ]
  Logic.Failed err ->
    mconcat
      [ if oldMemory.hasProgressBar
          then TextBuilders.moveCursorToLineStart <> TextBuilders.clearLine -- Clear progress bar
          else "",
        "\n",
        TextBuilders.boldRed "Error",
        ": ",
        to err.message,
        "\n",
        if null err.path
          then ""
          else
            mconcat
              [ "Location: ",
                TextBuilder.intercalateMap " > " to (reverse err.path),
                "\n"
              ],
        maybe "" (mappend "Suggestion: " . to . mappend "\n") err.suggestion,
        if null err.details
          then ""
          else
            mconcat
              [ "Details:\n",
                TextBuilder.intercalateMap
                  "\n"
                  ( \(key, value) ->
                      "  " <> to key <> ": " <> to value
                  )
                  err.details,
                "\n"
              ]
      ]
