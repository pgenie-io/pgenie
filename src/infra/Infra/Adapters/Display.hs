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
instance Logic.Reports (Fx Device Logic.Error) where
  enterStage path =
    runTotalIO \dev -> do
      progress <- (.progress) <$> readMVar dev.memoryVar
      Text.putStrLn
        $ from @TextBuilder
        $ mconcat
        $ [ "Progress: ",
            TextBuilderDev.doubleFixedPointPercent 0 progress,
            "%, at stage: ",
            TextBuilder.intercalateMap " > " to path
          ]

  exitStage _path progress =
    runTotalIO \dev -> do
      memory <- takeMVar dev.memoryVar
      let newProgress = memory.progress + progress
      let newMemory = memory {progress = newProgress}
      putMVar dev.memoryVar newMemory
