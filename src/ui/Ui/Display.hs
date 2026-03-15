module Ui.Display
  ( Display,
    new,
    handleEvent,
  )
where

import Data.Text.IO qualified as TextIO
import Logic.Algebra qualified as Logic
import Ui.Display.Components.Main qualified as Main
import Utils.Prelude

-- | Display device for managing progress and event rendering
data Display = Display
  { memoryVar :: MVar Main.Memory
  }

-- | Create a new Display instance
new :: IO Display
new = do
  currentTime <- getCurrentTime
  memoryVar <- newMVar (Main.init currentTime)
  pure Display {memoryVar}

-- | Handle a Logic event and update the display
handleEvent :: Display -> Logic.Event -> IO ()
handleEvent display event = do
  oldMemory <- takeMVar display.memoryVar
  currentTime <- getCurrentTime
  let (newMemory, output) = Main.update event currentTime oldMemory
  TextIO.hPutStr stderr (to output)
  hFlush stderr
  putMVar display.memoryVar newMemory
