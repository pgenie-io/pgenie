{-# LANGUAGE RequiredTypeArguments #-}

module ProgressLogic.Algebras.ProgressSpec where

import Base.Prelude
import ProgressLogic.Adapters.RecordEvents qualified
import ProgressLogic.Algebras.Progress
import Test.Hspec

spec :: Spec
spec = do
  it "" do
    let (events, result) =
          ProgressLogic.Adapters.RecordEvents.run do
            scenario
              ( proc a -> do
                  a <- stage "1" (pure . succ) -< a
                  a <- stage "2" (pure . succ) -< a
                  a <-
                    stage
                      "3"
                      ( \a ->
                          scenario
                            ( proc b -> do
                                b <- stage "3.1" (pure . succ) -< b
                                b <- stage "3.2" (pure . succ) -< b
                                b <- stage "3.3" (pure . succ) -< b
                                returnA -< b
                            )
                            a
                      )
                      -<
                        a
                  a <- stage "4" (pure . succ) -< a
                  returnA -< a
              )
              (0 :: Int)
    shouldBe result 6
    shouldBe events []
