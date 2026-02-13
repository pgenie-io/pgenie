module Infra.Adapters.Display
  ( Device,
    scope,
  )
where

import Base.Prelude
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Fx
import Logic qualified
import TextBuilder qualified
import TextBuilderDev qualified

data Device = Device
  { memoryVar :: MVar Memory
  }

data Memory = Memory
  { -- | Overall progress.
    progress :: Double,
    -- | Whether we've printed progress bar yet
    hasProgressBar :: Bool
  }

scope :: Fx.Scope Logic.Error Device
scope =
  acquire do
    memoryVar <- runTotalIO (\() -> newMVar Memory {progress = 0, hasProgressBar = False})
    pure Device {memoryVar}

-- | ANSI escape codes for terminal control
clearLine :: Text
clearLine = "\ESC[2K"

moveCursorToLineStart :: Text
moveCursorToLineStart = "\r"

-- | Render an ASCII progress bar
renderProgressBar :: Double -> Text
renderProgressBar progress =
  let barWidth = 46 -- Total width of the progress bar (between brackets)
      filledWidth = round (progress * fromIntegral barWidth)
      emptyWidth = barWidth - filledWidth
      filled = Text.replicate filledWidth "="
      arrow = if filledWidth > 0 && filledWidth < barWidth then ">" else ""
      empty = Text.replicate (emptyWidth - Text.length arrow) "."
      percentage = from @TextBuilder $ TextBuilderDev.doubleFixedPointPercent 1 progress
   in "[" <> filled <> arrow <> empty <> "]  " <> percentage

-- | Implementation of progress reporting with ASCII progress bar on last line
instance Logic.Emits (Fx Device Logic.Error) where
  emit event =
    case event of
      Logic.StageEntered path ->
        runTotalIO \dev -> do
          modifyMVar_ dev.memoryVar \memory -> do
            -- Do all IO while holding the MVar
            when memory.hasProgressBar $
              TextIO.putStr "\n"
            TextIO.putStr $ from @TextBuilder $ TextBuilder.intercalateMap " > " to (reverse path)
            TextIO.putStr "\n"
            hFlush stdout
            pure memory {hasProgressBar = False}
          
      Logic.StageExited _path progressDelta ->
        runTotalIO \dev -> do
          modifyMVar_ dev.memoryVar \memory -> do
            let newProgress = memory.progress + progressDelta
            -- Do all IO while holding the MVar
            when memory.hasProgressBar $
              TextIO.putStr $ moveCursorToLineStart <> clearLine
            TextIO.putStr $ renderProgressBar newProgress
            hFlush stdout
            pure memory {progress = newProgress, hasProgressBar = True}
          
      Logic.WarningEmitted err ->
        runTotalIO \dev -> do
          modifyMVar_ dev.memoryVar \memory -> do
            when memory.hasProgressBar $
              TextIO.putStr "\n"
            TextIO.putStr
              $ from @TextBuilder
              $ mconcat
              $ [ "Warning at location: ",
                  TextBuilder.intercalateMap " > " to (reverse err.path),
                  "\nMessage: ",
                  to err.message,
                  maybe "" (mappend "\nSuggestion: " . to) err.suggestion,
                  if null err.details
                    then ""
                    else
                      "\nDetails:\n"
                        <> TextBuilder.intercalateMap
                          "\n"
                          ( \(key, value) ->
                              "  " <> to key <> ": " <> to value
                          )
                          err.details
                ]
            TextIO.putStr "\n"
            hFlush stdout
            pure memory {hasProgressBar = False}
          
      Logic.Failed err ->
        runTotalIO \dev -> do
          withMVar dev.memoryVar \memory -> do
            when memory.hasProgressBar $
              TextIO.putStr "\n"
            TextIO.putStr
              $ from @TextBuilder
              $ mconcat
              $ [ "Failed at location: ",
                  TextBuilder.intercalateMap " > " to (reverse err.path),
                  "\nMessage: ",
                  to err.message,
                  maybe "" (mappend "\nSuggestion: " . to) err.suggestion,
                  if null err.details
                    then ""
                    else
                      "\nDetails:\n"
                        <> TextBuilder.intercalateMap
                          "\n"
                          ( \(key, value) ->
                              "  " <> to key <> ": " <> to value
                          )
                          err.details
                ]
            TextIO.putStr "\n"
            hFlush stdout
