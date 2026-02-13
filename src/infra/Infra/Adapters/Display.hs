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
          output <- modifyMVar dev.memoryVar \memory -> do
            -- Build the output string atomically
            let prefix = if memory.hasProgressBar 
                         then moveCursorToLineStart <> clearLine  -- Clear progress bar
                         else ""
                stageText = from @TextBuilder $ TextBuilder.intercalateMap " > " to (reverse path)
                fullOutput = prefix <> stageText <> "\n"
            pure (memory {hasProgressBar = False}, fullOutput)
          -- Single atomic write after releasing MVar
          TextIO.putStr output
          hFlush stdout
          
      Logic.StageExited _path progressDelta ->
        runTotalIO \dev -> do
          output <- modifyMVar dev.memoryVar \memory -> do
            let newProgress = memory.progress + progressDelta
            -- Build output string atomically
                clearAndBar = moveCursorToLineStart <> clearLine <> renderProgressBar newProgress
            pure (memory {progress = newProgress, hasProgressBar = True}, clearAndBar)
          -- Single atomic write after releasing MVar
          TextIO.putStr output
          hFlush stdout
          
      Logic.WarningEmitted err ->
        runTotalIO \dev -> do
          output <- modifyMVar dev.memoryVar \memory -> do
            let prefix = if memory.hasProgressBar 
                         then moveCursorToLineStart <> clearLine  -- Clear progress bar
                         else ""
                warningText = from @TextBuilder
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
                fullOutput = prefix <> warningText <> "\n"
            pure (memory {hasProgressBar = False}, fullOutput)
          TextIO.putStr output
          hFlush stdout
          
      Logic.Failed err ->
        runTotalIO \dev -> do
          needsPrefix <- withMVar dev.memoryVar (pure . (.hasProgressBar))
          let prefix = if needsPrefix 
                       then moveCursorToLineStart <> clearLine  -- Clear progress bar
                       else ""
              errorText = from @TextBuilder
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
              fullOutput = prefix <> errorText <> "\n"
          TextIO.putStr fullOutput
          hFlush stdout
