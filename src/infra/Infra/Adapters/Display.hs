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

display :: (Memory -> (TextBuilder, Memory)) -> Fx Device Logic.Error ()
display buildOutput =
  runTotalIO \dev -> do
    memory <- takeMVar dev.memoryVar
    let (output, newMemory) = buildOutput memory
    TextIO.putStr (to output)
    putMVar dev.memoryVar newMemory
    hFlush stdout

-- | Implementation of progress reporting with ASCII progress bar on last line
instance Logic.Emits (Fx Device Logic.Error) where
  emit event =
    case event of
      Logic.StageEntered path ->
        display \memory ->
          ( mconcat
              [ if memory.hasProgressBar
                  then TextBuilders.moveCursorToLineStart <> TextBuilders.clearLine
                  else "",
                TextBuilder.intercalateMap " > " to (reverse path),
                "\n",
                TextBuilders.progressBar memory.progress
              ],
            memory {hasProgressBar = True}
          )
      Logic.StageExited _path progressDelta ->
        display \memory ->
          if memory.progress >= 1
            then (mempty, memory) -- Don't update progress if already at 100%
            else
              let newProgress =
                    memory.progress + progressDelta
                  output =
                    mconcat
                      [ TextBuilders.moveCursorToLineStart,
                        TextBuilders.clearLine,
                        if newProgress < 1
                          then TextBuilders.progressBar newProgress
                          else "Done!\n"
                      ]
                  newMemory =
                    memory {progress = newProgress}
               in (output, newMemory)
      Logic.WarningEmitted err ->
        display \memory ->
          ( let prefix =
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
             in prefix <> warningText <> "\n",
            memory {hasProgressBar = False}
          )
      Logic.Failed err ->
        display \memory ->
          ( let prefix =
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
             in prefix <> errorText <> "\n",
            memory
          )
