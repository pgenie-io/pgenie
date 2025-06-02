module ProgressLogic.Algebras.Progress
  ( scenario,
    Scenario,
    stage,
  )
where

import Base.Prelude
import Control.Arrow
import ProgressLogic.Algebras.Reports

-- |
-- Statically structured execution scenario, which lets you observe progress.
data Scenario m i o
  = Scenario
      -- | Total amount of stages in the supplied action.
      Int
      -- | Total stages in the running action to offset in them to input to action.
      (Int -> Int -> i -> m o)

instance (Monad m) => Category (Scenario m) where
  id = Scenario 0 \_ _ -> pure
  Scenario total1 f1 . Scenario total2 f2 =
    Scenario (total1 + total2) \actualTotal offset input -> do
      output <- f2 actualTotal offset input
      f1 actualTotal (offset + total2) output

instance (Monad m) => Arrow (Scenario m) where
  arr f = Scenario 0 \_ _ input -> pure (f input)
  first (Scenario total f) =
    Scenario total \actualTotal offset (input, secondInput) -> do
      output <- f actualTotal offset input
      pure (output, secondInput)

instance (Monad m) => ArrowChoice (Scenario m) where
  left (Scenario total f) =
    Scenario total \actualTotal offset input -> case input of
      Left leftInput -> do
        output <- f actualTotal offset leftInput
        pure (Left output)
      Right rightInput -> pure (Right rightInput)

stage :: (Reports m) => Text -> (i -> m o) -> Scenario m i o
stage name action =
  Scenario 1 \actualTotal offset input -> do
    reportStageEnter name
    output <-
      action input
        & mapReports
          ( \report ->
              (fromIntegral offset + report)
                / fromIntegral actualTotal
          )
    reportStageExit name (fromIntegral (offset + 1) / fromIntegral actualTotal)
    pure output

scenario ::
  -- | Action to run.
  Scenario m i o ->
  -- | Input to the action.
  i ->
  -- | Result of the action.
  m o
scenario (Scenario total action) input = do
  action total 0 input
