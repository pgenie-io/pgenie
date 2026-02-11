module Infra.Adapters.Display where

import Base.Prelude
import Data.Text.IO qualified as Text
import Fx
import Logic qualified
import StagingAlgebra qualified
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

type Error = Void

-- | Temporary implementation of progress reporting. Just prints to console.
instance Logic.Reports (Fx Device Error) where
  enterStage path =
    (runTotalIO . const . Text.putStrLn . to . mconcat)
      [ "Entering stage: ",
        TextBuilder.intercalateMap " > " to path
      ]

  exitStage path progress =
    (runTotalIO . const . Text.putStrLn . to . mconcat)
      [ "Exiting stage: ",
        TextBuilder.intercalateMap " > " to path,
        " with progress: ",
        onto (show progress)
      ]

instance StagingAlgebra.Stages (Fx Device Error) where
  stage name substagesCount action = do
    result <-
      action
        & mapEnv
          ( \dev ->
              dev
                { progressPerStage =
                    dev.progressPerStage / fromIntegral substagesCount,
                  location =
                    name : dev.location
                }
          )

    -- Inner stage finished. Reporting progress.
    runTotalIO \dev -> do
      memory <- takeMVar dev.memoryVar
      let newProgress = memory.progress + dev.progressPerStage
      Text.putStrLn
        $ from @TextBuilder
        $ mconcat
        $ [ "Progress: ",
            TextBuilderDev.doubleFixedPointPercent 0 newProgress,
            "%, at stage: ",
            to name,
            ", location: ",
            TextBuilder.intercalateMap " > " to (reverse dev.location)
          ]
      let newMemory = memory {progress = newProgress}
      putMVar dev.memoryVar newMemory

    pure result
