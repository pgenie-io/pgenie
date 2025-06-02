{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module ProgressLogic.Algebras.RegRun where

import Base.Prelude
import Control.Arrow
import ProgressLogic.Algebras.Reports

data Stage m a = Stage Int (m a)

newtype Run m a = Run (Runtime -> m (a, Runtime))
  deriving
    (Functor, Applicative, Monad)
    via (StateT Runtime m)

instance (Reports m) => Reports (Run m) where
  reportStageEnter stageName =
    Run \runtime -> (,runtime) <$> reportStageEnter stageName
  reportStageExit stageName localProgress =
    Run \Runtime {..} ->
      (,Runtime
          { registered,
            finished = finished + 1
          })
        <$> reportStageExit
          stageName
          ((fromIntegral finished + localProgress) / fromIntegral registered)

data Runtime = Runtime
  { registered :: Int,
    finished :: Int
  }

-- | Lifts a Kleisli arrow in the base monad,
-- accounting for this computation in the overall progress.
--
-- Produces a Kleisli on the handle for a computation execution.
stage ::
  -- | Stage name.
  Text ->
  (a -> m b) ->
  Stage m (a -> Run m b)
stage stageName action =
  error "TODO"

summarize :: (Reports m) => Stage m (Run m a) -> Run m a
summarize =
  error "TODO"
