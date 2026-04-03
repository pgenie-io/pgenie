module Ui.Display.Components.MainSpec (spec) where

import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Logic.Algebra qualified as Logic
import Test.Hspec
import TextBuilder qualified
import Ui.Display.Components.Main qualified as Main
import Ui.Display.Components.Main.View qualified as View
import Utils.Prelude

-- | Fixed timestamp for deterministic tests.
t :: UTCTime
t = UTCTime (fromGregorian 2024 1 1) 0

-- | Fold a list of events through update, collecting all outputs.
runEvents :: [Logic.Event] -> [TextBuilder]
runEvents = go (Main.init t)
  where
    go _ [] = []
    go mem (e : es) =
      let (mem', out) = Main.update e t mem
       in out : go mem' es

-- | Check whether a TextBuilder output contains the "Done!" completion message.
containsDone :: TextBuilder -> Bool
containsDone tb = View.printDone == tb || Text.isInfixOf (TextBuilder.toText View.printDone) (TextBuilder.toText tb)

spec :: Spec
spec = do
  -- The infrastructure scope emits two StageExited events during startup:
  --   StageExited ["Starting Container"] 0.9  →  halved →  0.45
  --   StageExited ["Connecting"]         0.1  →  halved →  0.05
  -- Combined with analysis events (~0.375), the total accumulated progress
  -- approaches 0.875 before any generators run.  Each generator "Loading"
  -- leaf stage then contributes ~0.0417.  With 3 generators the progress
  -- exceeds 0.999 in the middle of the second generator's "Loading" stage,
  -- causing isDone to fire prematurely and "Done!" to be displayed while
  -- generators 2 and 3 are still running.
  describe "update" do
    it "does not show Done while generators are still in progress" do
      let -- Scope infrastructure events (already halved, as received by Main.update)
          scopeEvents =
            [ Logic.StageExited ["Starting Container"] 0.45
            , Logic.StageExited ["Connecting"] 0.05
            ]

          -- Analysis stage events (leaf contributions, already halved)
          analysisEvents =
            [ Logic.StageEntered ["Analysing"]
            , -- Migrations (Loading + Executing, 1 migration each)
              Logic.StageExited ["migrations/001.sql", "Loading", "Migrations", "Analysing"] 0.0625
            , Logic.StageExited ["migrations/001.sql", "Executing", "Migrations", "Analysing"] 0.0625
            , Logic.StageExited ["Migrations", "Analysing"] 0
            , -- Checking indexes
              Logic.StageExited ["Checking indexes", "Analysing"] 0.125
            , -- Queries (1 query)
              Logic.StageExited ["Loading", "query1", "Queries", "Analysing"] 0.0625
            , Logic.StageExited ["Inferring", "query1", "Queries", "Analysing"] 0.0625
            , Logic.StageExited ["Analysing"] 0
            ]

          -- 3 generators (each Loading + Compiling, already halved)
          gen1Events =
            [ Logic.StageEntered ["gen1", "Generating"]
            , Logic.StageExited ["Loading", "gen1", "Generating"] 0.0417
            , Logic.StageExited ["Compiling", "gen1", "Generating"] 0.0417
            , Logic.StageExited ["gen1", "Generating"] 0
            ]

          gen2PartialEvents =
            [ Logic.StageEntered ["gen2", "Generating"]
            , -- gen2 Loading completes — this is the event that pushes progress
              -- over 0.999 with the current buggy code, triggering isDone prematurely
              Logic.StageExited ["Loading", "gen2", "Generating"] 0.0417
            ]

          gen2RestEvents =
            [ Logic.StageExited ["Compiling", "gen2", "Generating"] 0.0417
            , Logic.StageExited ["gen2", "Generating"] 0
            ]

          gen3Events =
            [ Logic.StageEntered ["gen3", "Generating"]
            , Logic.StageExited ["Loading", "gen3", "Generating"] 0.0417
            , Logic.StageExited ["Compiling", "gen3", "Generating"] 0.0417
            , Logic.StageExited ["gen3", "Generating"] 0
            ]

          finalEvents =
            [ Logic.StageExited ["Generating"] 0
            , Logic.StageExited [] 0
            ]

          -- All events up to (and including) the moment gen3 is still running
          eventsBeforeAllDone =
            [ Logic.StageEntered [] ]
              <> scopeEvents
              <> analysisEvents
              <> [ Logic.StageEntered ["Generating"] ]
              <> gen1Events
              <> gen2PartialEvents
              -- gen2 is still compiling; gen3 hasn't started yet

          outputsBeforeAllDone = runEvents eventsBeforeAllDone

      -- None of the outputs before all generators complete should contain "Done!"
      mapM_
        (\out -> containsDone out `shouldBe` False)
        outputsBeforeAllDone

    it "shows Done exactly once, after the root stage exits" do
      let events =
            [ Logic.StageEntered []
            , Logic.StageExited ["Starting Container"] 0.45
            , Logic.StageExited ["Connecting"] 0.05
            , Logic.StageExited ["Analysing"] 0.375
            , Logic.StageEntered ["Generating"]
            , Logic.StageExited ["gen1", "Generating"] 0.0833
            , Logic.StageExited ["gen2", "Generating"] 0.0833
            , Logic.StageExited ["gen3", "Generating"] 0.0833
            , Logic.StageExited ["Generating"] 0
            , Logic.StageExited [] 0
            ]
          outputs = runEvents events
          doneOutputs = filter containsDone outputs

      -- Exactly one "Done!" should appear
      length doneOutputs `shouldBe` 1

      -- And it must be the very last output (from StageExited [] 0)
      last outputs `shouldSatisfy` containsDone
