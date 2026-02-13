module Infra.Adapters.Display
  ( Device,
    scope,
  )
where

import Base.Prelude
import Data.Text.IO qualified as Text
import Fx
import Logic qualified
import TextBuilder qualified
import TextBuilderDev qualified

data Device = Device
  { memoryVar :: MVar Memory,
    location :: [Text],
    progressPerStage :: Double
  }

data Memory = Memory
  { -- | Overall progress.
    progress :: Double
  }

scope :: Fx.Scope Logic.Error Device
scope =
  acquire do
    memoryVar <- runTotalIO (\() -> newMVar Memory {progress = 0})
    pure Device {memoryVar, location = [], progressPerStage = 1}

-- | Temporary implementation of progress reporting. Just prints to console.
instance Logic.Emits (Fx Device Logic.Error) where
  emit event =
    case event of
      Logic.StageEntered path ->
        runTotalIO \dev -> do
          progress <- (.progress) <$> readMVar dev.memoryVar
          Text.putStrLn
            $ from @TextBuilder
            $ mconcat
            $ [ TextBuilderDev.padFromLeft
                  7
                  ' '
                  (TextBuilderDev.doubleFixedPointPercent 0 progress <> " | "),
                TextBuilder.intercalateMap " > " to (reverse path)
              ]
      Logic.StageExited _path progress ->
        runTotalIO \dev -> do
          memory <- takeMVar dev.memoryVar
          let newProgress = memory.progress + progress
          let newMemory = memory {progress = newProgress}
          putMVar dev.memoryVar newMemory
      Logic.WarningEmitted err ->
        runTotalIO \_dev -> do
          Text.putStrLn
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
      Logic.Failed err ->
        runTotalIO \_dev -> do
          Text.putStrLn
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
