-- |
-- Terminal display device: holds the mutable render state across the run and
-- turns each runtime observation into output written to stderr.
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

-- | Construct a display with empty state, timestamped from the moment of creation.
new :: IO Display
new = do
  currentTime <- getCurrentTime
  memoryVar <- newMVar (Main.init currentTime)
  pure Display {memoryVar}

-- | Fold an observation into the display's state and immediately flush the
-- resulting output to stderr.
handleObservation :: Display -> Observing.Observation -> IO ()
handleObservation display observation = do
  modifyMVar_ display.memoryVar \oldMemory -> do
    currentTime <- getCurrentTime
    let (newMemory, output) = Main.update observation currentTime oldMemory
    TextIO.hPutStr stderr (to output)
    hFlush stderr
    pure newMemory
