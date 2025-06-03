module ReportingLogic.AlgebraSpec where

import Base.Prelude
import ReportingLogic.Adapters.RecordEvents qualified
import ReportingLogic.Algebra
import Test.Hspec

spec :: Spec
spec = do
  describe "Simulation 1" do
    let (events, result) =
          ReportingLogic.Adapters.RecordEvents.run
            let scenario1 = proc a -> do
                  a <- runStage "1" (pure . succ) -< a
                  a <- runStage "2" (pure . succ) -< a
                  a <- runStage "3" (runScenario scenario2) -< a
                  a <- runStage "4" (pure . succ) -< a
                  returnA -< a
                scenario2 = proc a -> do
                  a <- runStage "3.1" (pure . succ) -< a
                  a <- runStage "3.2" (pure . succ) -< a
                  a <- runStage "3.3" (pure . succ) -< a
                  returnA -< a
             in runScenario scenario1 (0 :: Int)

    describe "result" do
      it "Should be correct" do
        shouldBe result 6

    describe "events" do
      describe "progresses" do
        let progresses =
              events
                & mapMaybe \case
                  ReportingLogic.Adapters.RecordEvents.StageExit _ p -> Just p
                  _ -> Nothing
        it "Should be ascending" do
          shouldBe progresses (sort progresses)
        it "Should be larger than 0" do
          shouldSatisfy progresses (all (>= 0))
        it "Should be smaller than 1" do
          shouldSatisfy progresses (all (<= 1))
        it "Should have the same length as the amount of stages" do
          shouldBe (length progresses) 7
        describe "deduplicated" do
          let deduplicated = nub progresses
          describe "length" do
            let deduplicatedLength = length deduplicated
            it "Should be larger than 1" do
              shouldSatisfy deduplicatedLength (> 1)
