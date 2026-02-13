module Infra.Adapters.Display
  ( Device,
    scope,
  )
where

import Base.Prelude
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Fx
import Infra.Adapters.Display.TextBuilders qualified as TextBuilders
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

-- | Implementation of progress reporting with ASCII progress bar on last line
instance Logic.Emits (Fx Device Logic.Error) where
  emit event =
    case event of
      Logic.StageEntered path ->
        runTotalIO \dev -> do
          memory <- takeMVar dev.memoryVar
          let prefix =
                if memory.hasProgressBar
                  then TextBuilders.moveCursorToLineStart <> TextBuilders.clearLine -- Clear progress bar
                  else ""
              stageText = to (TextBuilder.intercalateMap " > " to (reverse path))
              output = to (prefix <> stageText <> "\n")
          TextIO.putStr output
          putMVar dev.memoryVar (memory {hasProgressBar = False})
          hFlush stdout
      Logic.StageExited _path progressDelta ->
        runTotalIO \dev -> do
          memory <- takeMVar dev.memoryVar
          let newProgress = memory.progress + progressDelta
              -- Build output string atomically
              output = to (TextBuilders.moveCursorToLineStart <> TextBuilders.clearLine <> TextBuilders.progressBar 20 newProgress)
          TextIO.putStr output
          putMVar dev.memoryVar (memory {progress = newProgress, hasProgressBar = True})
          hFlush stdout
      Logic.WarningEmitted err ->
        runTotalIO \dev -> do
          memory <- takeMVar dev.memoryVar

          TextIO.putStr
            let prefix =
                  if memory.hasProgressBar
                    then TextBuilders.moveCursorToLineStart <> TextBuilders.clearLine -- Clear progress bar
                    else ""
                warningText =
                  from @TextBuilder
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
             in to (prefix <> warningText <> "\n")
          putMVar dev.memoryVar (memory {hasProgressBar = False})
          hFlush stdout
      Logic.Failed err ->
        runTotalIO \dev -> do
          memory <- takeMVar dev.memoryVar

          TextIO.putStr
            let prefix =
                  if memory.hasProgressBar
                    then TextBuilders.moveCursorToLineStart <> TextBuilders.clearLine -- Clear progress bar
                    else ""
                errorText =
                  mconcat
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
             in to (prefix <> errorText <> "\n")
          putMVar dev.memoryVar memory
          hFlush stdout
