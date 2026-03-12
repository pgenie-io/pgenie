module Ui.Display.Components.ProgressBar
  ( Memory,
    init,
    update,
    isDone,
  )
where

import Base.Prelude hiding (init)
import Ui.Display.Components.ProgressBar.View qualified as View

data Memory = Memory
  { progress :: Double,
    startTime :: UTCTime,
    timeLeftEstimate :: Maybe NominalDiffTime
  }
  deriving stock (Eq, Show)

init :: UTCTime -> Memory
init startTime =
  Memory
    { progress = 0,
      startTime,
      timeLeftEstimate = Nothing
    }

isDone :: Memory -> Bool
isDone mem = mem.progress >= 1

-- | Advance progress by the given delta, update the time estimate, and render.
update :: Double -> UTCTime -> Memory -> (Memory, TextBuilder)
update delta currentTime mem =
  let newProgress =
        mem.progress + delta
      newProgressNormalized =
        if newProgress >= 0.999
          then 1
          else newProgress
      elapsedTime =
        currentTime `diffUTCTime` mem.startTime
      newTimeLeftEstimate =
        if newProgressNormalized > 0
          then Just (elapsedTime / realToFrac newProgressNormalized - elapsedTime)
          else Nothing
      mem' =
        mem {progress = newProgressNormalized, timeLeftEstimate = newTimeLeftEstimate}
   in (mem', View.render newProgressNormalized)
