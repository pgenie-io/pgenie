-- |
-- Behavioural tests for the main display component: breadcrumb rendering and
-- when the "Done!" completion message is (and isn't) shown.
module Ui.Display.Components.MainSpec (spec) where

import Data.Text qualified as Text
import Interpreters.Observing qualified as Observing
import Test.Hspec
import TextBuilder qualified
import Ui.Display.Components.Main qualified as Main
import Ui.Display.Components.Main.View qualified as View
import Utils.Prelude

-- | Fixed timestamp for deterministic tests.
t :: UTCTime
t = UTCTime (fromGregorian 2024 1 1) 0

-- | Fold a list of events through update, collecting all outputs.
runEvents :: [Observing.Observation] -> [TextBuilder]
runEvents = go (Main.init t)
  where
    go _ [] = []
    go mem (e : es) =
      let (mem', out) = Main.update e t mem
       in out : go mem' es

-- | Check whether a TextBuilder output contains the "Done!" completion message.
containsDone :: TextBuilder -> Bool
containsDone tb = View.printDone == tb || Text.isInfixOf (TextBuilder.toText View.printDone) (TextBuilder.toText tb)

-- | Test suite for "Ui.Display.Components.Main".
spec :: Spec
spec = do
  describe "stage rendering" do
    it "renders the full breadcrumb when a nested stage exits" do
      TextBuilder.toText (View.printStageDone ["Loading", "haskell", "Generating"])
        `shouldBe` "Generating > haskell > Loading > \ESC[1;32mDone\ESC[0m\n"

    it "renders a top-level stage exit with the stage name" do
      TextBuilder.toText (View.printStageDone ["Generating"])
        `shouldBe` "Generating > \ESC[1;32mDone\ESC[0m\n"

  describe "progress handling" do
    it "shows stage completion with green Done when a non-root stage exits" do
      let outputs =
            runEvents
              [ Observing.StageEntered ["Loading", "haskell", "Generating"],
                Observing.StageExited ["Loading", "haskell", "Generating"] 0.0417
              ]

          containsStageDone tb = Text.isInfixOf "Done" (TextBuilder.toText tb)

      any containsStageDone outputs `shouldBe` True

    it "does not show Done while generators are still in progress" do
      let -- Scope infrastructure events (already halved, as received by Main.update)
          scopeEvents =
            [ Observing.StageExited ["Starting Container"] 0.45,
              Observing.StageExited ["Connecting"] 0.05
            ]

          -- Analysis stage events (leaf contributions, already halved)
          analysisEvents =
            [ Observing.StageEntered ["Analysing"],
              -- Migrations (Loading + Executing, 1 migration each)
              Observing.StageExited ["migrations/001.sql", "Loading", "Migrations", "Analysing"] 0.0625,
              Observing.StageExited ["migrations/001.sql", "Executing", "Migrations", "Analysing"] 0.0625,
              Observing.StageExited ["Migrations", "Analysing"] 0,
              -- Checking indexes
              Observing.StageExited ["Checking indexes", "Analysing"] 0.125,
              -- Queries (1 query)
              Observing.StageExited ["Loading", "query1", "Queries", "Analysing"] 0.0625,
              Observing.StageExited ["Inferring", "query1", "Queries", "Analysing"] 0.0625,
              Observing.StageExited ["Analysing"] 0
            ]

          -- 3 generators (each Loading + Compiling, already halved)
          gen1Events =
            [ Observing.StageEntered ["gen1", "Generating"],
              Observing.StageExited ["Loading", "gen1", "Generating"] 0.0417,
              Observing.StageExited ["Compiling", "gen1", "Generating"] 0.0417,
              Observing.StageExited ["gen1", "Generating"] 0
            ]

          gen2PartialEvents =
            [ Observing.StageEntered ["gen2", "Generating"],
              -- gen2 Loading completes while later generators are still compiling.
              Observing.StageExited ["Loading", "gen2", "Generating"] 0.0417
            ]

          -- All events up to (and including) the moment gen3 is still running
          eventsBeforeAllDone =
            [Observing.StageEntered []]
              <> scopeEvents
              <> analysisEvents
              <> [Observing.StageEntered ["Generating"]]
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
            [ Observing.StageEntered [],
              Observing.StageExited ["Starting Container"] 0.45,
              Observing.StageExited ["Connecting"] 0.05,
              Observing.StageExited ["Analysing"] 0.375,
              Observing.StageEntered ["Generating"],
              Observing.StageExited ["gen1", "Generating"] 0.0833,
              Observing.StageExited ["gen2", "Generating"] 0.0833,
              Observing.StageExited ["gen3", "Generating"] 0.0833,
              Observing.StageExited ["Generating"] 0,
              Observing.StageExited [] 0
            ]
          outputs = runEvents events
          doneOutputs = filter containsDone outputs

      -- Exactly one "Done!" should appear
      length doneOutputs `shouldBe` 1

      -- And it must be the very last output (from StageExited [] 0)
      last outputs `shouldSatisfy` containsDone
