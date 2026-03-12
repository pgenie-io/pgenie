module Ui.Display.Components.Main
  ( Memory,
    init,
    update,
  )
where

import Base.Prelude hiding (init)
import Logic.Algebra qualified as Logic
import TextBuilder
import Ui.Display.Components.Main.View qualified as View

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

-- | Combined update: transitions state and produces terminal output
update :: Logic.Event -> UTCTime -> Memory -> (Memory, TextBuilder)
update event currentTime mem = case event of
  Logic.StageEntered path ->
    if mem.progress >= 1
      then (mem, mempty)
      else
        let mem' = setCurrentTime currentTime mem {hasProgressBar = True}
         in ( mem',
              mconcat
                [ if mem.hasProgressBar then View.eraseLine else mempty,
                  View.printStagePath path,
                  View.printProgressBar mem'.progress
                ]
            )
  Logic.StageExited path progressDelta ->
    if mem.progress >= 1 || not mem.hasProgressBar
      then (setCurrentTime currentTime mem, mempty)
      else
        let newProgress = let p = mem.progress + progressDelta in if p >= 0.999 then 1 else p
            mem' = setCurrentTime currentTime mem {progress = newProgress}
         in ( mem',
              if newProgress >= 1 || null path
                then View.printDone
                else View.eraseLine <> View.printProgressBar newProgress
            )
  Logic.WarningEmitted err ->
    let mem' = setCurrentTime currentTime mem {hasProgressBar = True}
     in ( mem',
          mconcat
            [ if mem.hasProgressBar then View.eraseLine else mempty,
              View.printWarning err.path err.message err.suggestion err.details mem'.progress
            ]
        )
  Logic.Failed err ->
    let mem' = setCurrentTime currentTime mem {hasProgressBar = False}
     in ( mem',
          mconcat
            [ if mem.hasProgressBar then View.eraseLine else mempty,
              View.printError err.path err.message err.suggestion err.details
            ]
        )

setCurrentTime :: UTCTime -> Memory -> Memory
setCurrentTime currentTime memory =
  let elapsedTime = currentTime `diffUTCTime` memory.startTime
      newTimeLeftEstimate =
        if memory.progress > 0
          then Just (elapsedTime / realToFrac memory.progress - elapsedTime)
          else Nothing
   in memory {timeLeftEstimate = newTimeLeftEstimate}
