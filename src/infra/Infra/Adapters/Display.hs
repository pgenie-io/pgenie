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

-- | Update the progress bar on the last line
updateProgressBar :: Double -> Bool -> IO ()
updateProgressBar progress hasBar = do
  when hasBar $ do
    -- If there's already a progress bar, clear it
    TextIO.putStr $ moveCursorToLineStart <> clearLine
  TextIO.putStr $ renderProgressBar progress
  hFlush stdout

-- | Implementation of progress reporting with ASCII progress bar on last line
instance Logic.Emits (Fx Device Logic.Error) where
  emit event =
    case event of
      Logic.StageEntered path ->
        runTotalIO \dev -> do
          memory <- takeMVar dev.memoryVar
          -- Clear the progress bar if it exists
          when memory.hasProgressBar $ do
            TextIO.putStr $ moveCursorToLineStart <> clearLine
          -- Print the new stage on a new line
          TextIO.putStrLn $ from @TextBuilder $ TextBuilder.intercalateMap " > " to (reverse path)
          hFlush stdout
          -- Update to show we'll need to clear the progress bar next time
          let newMemory = memory {hasProgressBar = False}
          putMVar dev.memoryVar newMemory
          
      Logic.StageExited _path progressDelta ->
        runTotalIO \dev -> do
          memory <- takeMVar dev.memoryVar
          let newProgress = memory.progress + progressDelta
          let newMemory = memory {progress = newProgress, hasProgressBar = True}
          putMVar dev.memoryVar newMemory
          updateProgressBar newProgress memory.hasProgressBar
          
      Logic.WarningEmitted err ->
        runTotalIO \dev -> do
          memory <- takeMVar dev.memoryVar
          -- Clear the progress bar before printing warning
          when memory.hasProgressBar $ do
            TextIO.putStr $ moveCursorToLineStart <> clearLine
          
          TextIO.putStrLn
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
          hFlush stdout
          
          -- Update to show we need to redraw progress bar
          let newMemory = memory {hasProgressBar = False}
          putMVar dev.memoryVar newMemory
          
      Logic.Failed err ->
        runTotalIO \dev -> do
          memory <- readMVar dev.memoryVar
          -- Clear the progress bar before printing error
          when memory.hasProgressBar $ do
            TextIO.putStr $ moveCursorToLineStart <> clearLine
          
          TextIO.putStrLn
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
          hFlush stdout
