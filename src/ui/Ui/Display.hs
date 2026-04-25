module Ui.Display
  ( Display,
    new,
    handleObservation,
  )
where

import Data.Text.IO qualified as TextIO
import Interpreters.Observing qualified as Observing
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

-- | Handle a runtime observation and update the display.
handleObservation :: Display -> Observing.Observation -> IO ()
handleObservation display observation = do
  oldMemory <- takeMVar display.memoryVar
  currentTime <- getCurrentTime
  let (newMemory, output) = Main.update observation currentTime oldMemory
  TextIO.hPutStr stderr (to output)
  hFlush stderr
  putMVar display.memoryVar newMemory
