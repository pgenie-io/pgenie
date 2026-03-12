module Ui.Display.Memory
  ( Memory (..),
    init,
    update,
    setCurrentTime,
  )
where

import Base.Prelude hiding (init)
import Logic.Algebra qualified as Logic

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

-- | Memory function: event -> model -> model
-- Pure state transition logic following Elm architecture
update :: Logic.Event -> Memory -> Memory
update event memory =
  case event of
    Logic.StageEntered _path ->
      memory
        { hasProgressBar = True
        }
    Logic.StageExited _path progressDelta ->
      if memory.progress >= 1
        then memory -- Don't update progress if already at 100%
        else
          let newProgress = memory.progress + progressDelta
              newProgressCapped = if newProgress >= 0.999 then 1 else newProgress -- Avoid floating point imprecision issues
           in memory
                { progress = newProgressCapped
                }
    Logic.WarningEmitted _err ->
      memory
        { hasProgressBar = True
        }
    Logic.Failed _err ->
      memory
        { hasProgressBar = False
        }

setCurrentTime :: UTCTime -> Memory -> Memory
setCurrentTime currentTime memory =
  let elapsedTime = currentTime `diffUTCTime` memory.startTime
      newTimeLeftEstimate =
        if memory.progress > 0
          then Just (elapsedTime / realToFrac memory.progress - elapsedTime)
          else Nothing
   in memory {timeLeftEstimate = newTimeLeftEstimate}
