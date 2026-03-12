module Ui.Display.Components.Main
  ( Memory,
    init,
    update,
  )
where

import Base.Prelude hiding (init)
import Logic.Algebra qualified as Logic
import Ui.Display.Components.Main.View qualified as View
import Ui.Display.Components.ProgressBar qualified as ProgressBar

-- | Display state
data Memory = Memory
  { startTime :: UTCTime,
    progressBar :: Maybe ProgressBar.Memory
  }
  deriving stock (Eq, Show)

init :: UTCTime -> Memory
init startTime =
  Memory
    { startTime,
      progressBar = Nothing
    }

-- | Combined update: transitions state and produces terminal output
update :: Logic.Event -> UTCTime -> Memory -> (Memory, TextBuilder)
update event currentTime mem = case event of
  Logic.StageEntered path ->
    case mem.progressBar of
      Just pb | ProgressBar.isDone pb -> (mem, mempty)
      _ ->
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
  Logic.StageExited path progressDelta ->
    case mem.progressBar of
      Nothing -> (mem, mempty)
      Just pb | ProgressBar.isDone pb -> (mem, mempty)
      Just pb ->
        let (pb', pbOutput) = ProgressBar.update progressDelta currentTime pb
            mem' = mem {progressBar = Just pb'}
         in ( mem',
              if ProgressBar.isDone pb' || null path
                then View.printDone
                else View.eraseLine <> pbOutput
            )
  Logic.WarningEmitted err ->
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
  Logic.Failed err ->
    ( mem {progressBar = Nothing},
      mconcat
        [ if isJust mem.progressBar then View.eraseLine else mempty,
          View.printError err.path err.message err.suggestion err.details
        ]
    )
