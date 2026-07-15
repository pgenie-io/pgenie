-- |
-- Progress-bar sub-component: tracks fractional completion and a
-- moving time-remaining estimate, and renders both to a terminal line.
module Ui.Display.Components.ProgressBar
  ( Memory,
    init,
    update,
    isDone,
  )
where

import Ui.Display.Components.ProgressBar.View qualified as View
import Utils.Prelude hiding (init)

-- | Progress-bar state: fraction complete so far, when tracking started, and
-- the current estimate of time remaining (unavailable until progress > 0).
data Memory = Memory
  { progress :: Double,
    startTime :: UTCTime,
    timeLeftEstimate :: Maybe NominalDiffTime
  }
  deriving stock (Eq, Show)

-- | Fresh progress-bar state, timed from the given start, with no estimate yet.
init :: UTCTime -> Memory
init startTime =
  Memory
    { progress = 0,
      startTime,
      timeLeftEstimate = Nothing
    }

-- | True once progress has reached (or exceeded) 100%.
isDone :: Memory -> Bool
isDone mem = mem.progress >= 1

-- | Advance progress by the given delta, update the time estimate, and render.
update :: Double -> UTCTime -> Memory -> (Memory, TextBuilder)
update delta currentTime mem =
  let newProgress =
        mem.progress + delta
      newProgressNormalized =
        min 0.999 newProgress
      elapsedTime =
        currentTime `diffUTCTime` mem.startTime
      newTimeLeftEstimate =
        if newProgressNormalized > 0
          then Just (elapsedTime / realToFrac newProgressNormalized - elapsedTime)
          else Nothing
      mem' =
        mem {progress = newProgressNormalized, timeLeftEstimate = newTimeLeftEstimate}
   in (mem', View.render newProgressNormalized)
