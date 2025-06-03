module ProgressLogic.Algebras.ProgressSpec where

import Base.Prelude
import ProgressLogic.Adapters.RecordEvents qualified
import ProgressLogic.Algebras.Progress
import Test.Hspec

spec :: Spec
spec = do
  it "" do
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
        (events, result) =
          ProgressLogic.Adapters.RecordEvents.run do
            runScenario scenario1 (0 :: Int)
    shouldBe result 6
    shouldBe events []
