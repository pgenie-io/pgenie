module Infra.Adapters.Display.Memory
  ( Memory (..),
    init,
    update,
  )
where

import Base.Prelude hiding (init)
import Logic.Algebra qualified as Logic

-- | Display state
data Memory = Memory
  { -- | Overall progress.
    progress :: Double,
    -- | Whether we've printed progress bar yet
    hasProgressBar :: Bool
  }

init :: Memory
init =
  Memory
    { progress = 0,
      hasProgressBar = False
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
          memory
            { progress = memory.progress + progressDelta
            }
    Logic.WarningEmitted _err ->
      memory
        { hasProgressBar = False
        }
    Logic.Failed _err ->
      memory
