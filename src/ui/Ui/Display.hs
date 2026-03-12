module Ui.Display
  ( Display,
    new,
    handleEvent,
  )
where

import Base.Prelude
import Data.Text.IO qualified as TextIO
import Logic.Algebra qualified as Logic
import Ui.Display.Memory qualified as Memory
import Ui.Display.View qualified as View

-- | Display device for managing progress and event rendering
data Display = Display
  { memoryVar :: MVar Memory.Memory
  }

-- | Create a new Display instance
new :: IO Display
new = do
  currentTime <- getCurrentTime
  memoryVar <- newMVar (Memory.init currentTime)
  pure Display {memoryVar}

-- | Handle a Logic event and update the display
handleEvent :: Display -> Logic.Event -> IO ()
handleEvent display event = do
  oldMemory <- takeMVar display.memoryVar
  currentTime <- getCurrentTime
  let newMemory = Memory.setCurrentTime currentTime (Memory.update event oldMemory)
      output = View.view event oldMemory newMemory
  TextIO.hPutStr stderr (to output)
  hFlush stderr
  putMVar display.memoryVar newMemory
