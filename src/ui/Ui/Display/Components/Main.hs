module Ui.Display.Components.Main
  ( Memory,
    init,
    update,
  )
where

import Interpreters.Observing qualified as Observing
import Logic qualified
import Ui.Display.Components.Main.View qualified as View
import Ui.Display.Components.ProgressBar qualified as ProgressBar
import Utils.Prelude hiding (init)

-- | Display state
data Memory = Memory
  { startTime :: UTCTime,
    progressBar :: Maybe ProgressBar.Memory,
    isCompleted :: Bool
  }
  deriving stock (Eq, Show)

init :: UTCTime -> Memory
init startTime =
  Memory
    { startTime,
      progressBar = Nothing,
      isCompleted = False
    }

-- | Combined update: transitions state and produces terminal output
update :: Observing.Observation -> UTCTime -> Memory -> (Memory, TextBuilder)
update observation currentTime mem =
  if mem.isCompleted
    then (mem, mempty)
    else case observation of
      Observing.StageEntered path ->
        let (pb', pbOutput) =
              ProgressBar.update
                0
                currentTime
                (fromMaybe (ProgressBar.init mem.startTime) mem.progressBar)
            mem' = mem {progressBar = Just pb'}
         in ( mem',
              mconcat
                [ if isJust mem.progressBar then View.eraseLine else mempty,
                  View.printStagePath path,
                  pbOutput
                ]
            )
      Observing.StageExited path progressDelta ->
        case mem.progressBar of
          Nothing -> (mem, mempty)
          Just pb ->
            let (pb', pbOutput) = ProgressBar.update progressDelta currentTime pb
                mem' = mem {progressBar = Just pb'}
             in if null path
                  then (mem' {isCompleted = True}, View.printDone)
                  else (mem', View.eraseLine <> pbOutput)
      Observing.WarningEmitted err ->
        let (pb', pbOutput) =
              ProgressBar.update
                0
                currentTime
                (fromMaybe (ProgressBar.init mem.startTime) mem.progressBar)
            mem' = mem {progressBar = Just pb'}
         in ( mem',
              mconcat
                [ if isJust mem.progressBar then View.eraseLine else mempty,
                  View.printWarning err.path err.message err.suggestion err.details,
                  pbOutput
                ]
            )
      Observing.ExecutionFailed err ->
        ( mem {progressBar = Nothing},
          mconcat
            [ if isJust mem.progressBar then View.eraseLine else mempty,
              View.printError err.path err.message err.suggestion err.details
            ]
        )
