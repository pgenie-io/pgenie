module App.Runtimes.Displaying
  ( Displaying,
  )
where

import App.Runtimes.EmittingEvents qualified as Runtimes.EmittingEvents
import App.Services.Display qualified as Services.Display
import Base.Prelude
import Data.Text.IO qualified as Text
import ParallelismAlgebra
import StagingAlgebra

type Displaying = ReaderT Services.Display.Context

instance (MonadIO m) => Runtimes.EmittingEvents.Reports (Displaying m) where
  enterStage name =
    (liftIO . Text.putStrLn . mconcat)
      [ "Entering stage: ",
        name
      ]

  exitStage name progress =
    (liftIO . Text.putStrLn . mconcat)
      [ "Exiting stage: ",
        name,
        " with progress: ",
        onto (show progress)
      ]
