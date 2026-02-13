module Infra.Adapters.Display
  ( Device,
    scope,
  )
where

import Base.Prelude
import Data.Text.IO qualified as TextIO
import Fx
import Infra.Adapters.Display.Memory qualified as Memory
import Infra.Adapters.Display.View qualified as View
import Logic.Algebra qualified as Logic

data Device = Device
  { memoryVar :: MVar Memory.Memory
  }

scope :: Fx.Scope Logic.Error Device
scope =
  acquire do
    memoryVar <- runTotalIO (\() -> newMVar Memory.init)
    pure Device {memoryVar}

-- | Implementation of progress reporting with ASCII progress bar on last line
instance Logic.Emits (Fx Device Logic.Error) where
  emit event =
    runTotalIO \dev -> do
      oldMemory <- takeMVar dev.memoryVar
      let newMemory = Memory.update event oldMemory
          output = View.view event oldMemory newMemory
      TextIO.putStr (to output)
      putMVar dev.memoryVar newMemory
      hFlush stdout
