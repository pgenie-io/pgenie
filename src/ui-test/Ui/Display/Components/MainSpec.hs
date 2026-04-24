module Ui.Display.Components.MainSpec (spec) where

import Data.Text qualified as Text
import Infra.Adapters.Script qualified as Script
import Test.Hspec
import TextBuilder qualified
import Ui.Display.Components.Main qualified as Main
import Ui.Display.Components.Main.View qualified as View
import Utils.Prelude

-- | Fixed timestamp for deterministic tests.
t :: UTCTime
t = UTCTime (fromGregorian 2024 1 1) 0

-- | Fold a list of events through update, collecting all outputs.
runEvents :: [Script.Event] -> [TextBuilder]
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
  describe "progress handling" do
    it "does not show Done while generators are still in progress" do
      let -- Scope infrastructure events (already halved, as received by Main.update)
          scopeEvents =
            [ Script.StageExited ["Starting Container"] 0.45,
              Script.StageExited ["Connecting"] 0.05
            ]

          -- Analysis stage events (leaf contributions, already halved)
          analysisEvents =
            [ Script.StageEntered ["Analysing"],
              -- Migrations (Loading + Executing, 1 migration each)
              Script.StageExited ["migrations/001.sql", "Loading", "Migrations", "Analysing"] 0.0625,
              Script.StageExited ["migrations/001.sql", "Executing", "Migrations", "Analysing"] 0.0625,
              Script.StageExited ["Migrations", "Analysing"] 0,
              -- Checking indexes
              Script.StageExited ["Checking indexes", "Analysing"] 0.125,
              -- Queries (1 query)
              Script.StageExited ["Loading", "query1", "Queries", "Analysing"] 0.0625,
              Script.StageExited ["Inferring", "query1", "Queries", "Analysing"] 0.0625,
              Script.StageExited ["Analysing"] 0
            ]

          -- 3 generators (each Loading + Compiling, already halved)
          gen1Events =
            [ Script.StageEntered ["gen1", "Generating"],
              Script.StageExited ["Loading", "gen1", "Generating"] 0.0417,
              Script.StageExited ["Compiling", "gen1", "Generating"] 0.0417,
              Script.StageExited ["gen1", "Generating"] 0
            ]

          gen2PartialEvents =
            [ Script.StageEntered ["gen2", "Generating"],
              -- gen2 Loading completes while later generators are still compiling.
              Script.StageExited ["Loading", "gen2", "Generating"] 0.0417
            ]

          -- All events up to (and including) the moment gen3 is still running
          eventsBeforeAllDone =
            [Script.StageEntered []]
              <> scopeEvents
              <> analysisEvents
              <> [Script.StageEntered ["Generating"]]
              <> gen1Events
              <> gen2PartialEvents
          -- gen2 is still compiling; gen3 hasn't started yet

          outputsBeforeAllDone = runEvents eventsBeforeAllDone

      -- None of the outputs before all generators complete should contain "Done!"
      mapM_
        (\out -> containsDone out `shouldBe` False)
        outputsBeforeAllDone

      -- And none should claim 100% before the final root completion event.
      mapM_
        (\out -> Text.isInfixOf "100.0%" (TextBuilder.toText out) `shouldBe` False)
        outputsBeforeAllDone

    it "shows Done exactly once, after the root stage exits" do
      let events =
            [ Script.StageEntered [],
              Script.StageExited ["Starting Container"] 0.45,
              Script.StageExited ["Connecting"] 0.05,
              Script.StageExited ["Analysing"] 0.375,
              Script.StageEntered ["Generating"],
              Script.StageExited ["gen1", "Generating"] 0.0833,
              Script.StageExited ["gen2", "Generating"] 0.0833,
              Script.StageExited ["gen3", "Generating"] 0.0833,
              Script.StageExited ["Generating"] 0,
              Script.StageExited [] 0
            ]
          outputs = runEvents events
          doneOutputs = filter containsDone outputs

      -- Exactly one "Done!" should appear
      length doneOutputs `shouldBe` 1

      -- And it must be the very last output (from StageExited [] 0)
      last outputs `shouldSatisfy` containsDone
