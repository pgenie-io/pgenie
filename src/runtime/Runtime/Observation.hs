module Runtime.Observation
  ( Observation (..),
  )
where

import Logic qualified
import Utils.Prelude

-- | Observations produced while executing capability-based logic.
data Observation
  = StageEntered [Text]
  | StageExited [Text] Double
  | WarningEmitted Logic.Report
  | ExecutionFailed Logic.Report
  deriving stock (Eq, Show)
