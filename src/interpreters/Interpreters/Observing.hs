-- | Pure interpreter for staged logic that emits structured observations
-- for a host environment to render or record.
module Interpreters.Observing
  ( Observation (..),
    Observes (..),
    Observing,
    interpretObserving,
  )
where

import Control.Monad.Parallel qualified as MonadParallel
import Data.Text qualified as Text
import Logic qualified
import Logic.Report qualified as Report
import Utils.Prelude hiding (readFile, writeFile)

-- | Observations produced while executing capability-based logic.
data Observation
  = StageEntered [Text]
  | StageExited [Text] Double
  | WarningEmitted Logic.Report
  | ExecutionFailed Logic.Report
  deriving stock (Eq, Show)

-- | Capability to publish runtime observations.
class (Monad m) => Observes m where
  observe :: Observation -> m ()

-- | Transformer that tracks the current stage path and per-stage progress
-- budget, implementing the 'Logic.Stages' and 'Logic.Warns' capabilities.
newtype Observing m a = Observing (Double -> [Text] -> m a)

-- | Interpret an 'Observing' action at the top level with full progress budget.
interpretObserving :: Observing m a -> m a
interpretObserving (Observing f) = f 1 []

instance (Functor m) => Functor (Observing m) where
  fmap f (Observing g) = Observing \progress path ->
    fmap f (g progress path)

instance (Applicative m) => Applicative (Observing m) where
  pure a = Observing \_ _ -> pure a
  Observing f <*> Observing a = Observing \progress path ->
    f progress path <*> a progress path

instance (Monad m) => Monad (Observing m) where
  Observing ma >>= f = Observing \progress path -> do
    a <- ma progress path
    let Observing mb = f a
    mb progress path

instance (MonadParallel.MonadParallel m) => MonadParallel.MonadParallel (Observing m) where
  bindM2 f (Observing ma) (Observing mb) = Observing \progress path ->
    MonadParallel.bindM2
      (\a b -> let Observing mc = f a b in mc progress path)
      (ma progress path)
      (mb progress path)

instance (MonadError Logic.Report m) => MonadError Logic.Report (Observing m) where
  throwError e = Observing \_ path ->
    throwError (Report.nest path e)

  catchError (Observing f) handler =
    Observing \progress path -> catchError (f progress path) (\e -> let Observing h = handler e in h progress path)

instance (Observes m, Monad m) => Logic.Stages (Observing m) where
  stage name substagesCount (Observing runInner) =
    Observing \progressPerStage path -> do
      let newPath =
            if Text.null name
              then path
              else name : path
      observe (StageEntered newPath)
      (remainingProgress, result) <-
        if substagesCount > 0
          then (0,) <$> runInner (progressPerStage / fromIntegral substagesCount) newPath
          else (progressPerStage,) <$> runInner 0 newPath
      observe (StageExited newPath remainingProgress)
      pure result

instance (Observes m, Monad m) => Logic.Warns (Observing m) where
  warn report = Observing \_ path -> observe (WarningEmitted (Report.nest path report))

instance (Logic.DbOps m) => Logic.DbOps (Observing m) where
  executeMigration sql = hoist (Logic.executeMigration sql)
  inferQueryTypes sql = hoist (Logic.inferQueryTypes sql)
  explainQuery sql = hoist (Logic.explainQuery sql)
  getIndexes = hoist Logic.getIndexes

instance (Logic.FsOps m) => Logic.FsOps (Observing m) where
  readFile path = hoist (Logic.readFile path)
  writeFile path content = hoist (Logic.writeFile path content)
  listDir path = hoist (Logic.listDir path)

instance (Logic.LoadsGen m) => Logic.LoadsGen (Observing m) where
  loadGen loc hash = hoist (Logic.loadGen loc hash)

instance (Logic.LoadsProjectFile m) => Logic.LoadsProjectFile (Observing m) where
  loadProjectFile = Observing \_ _ -> Logic.loadProjectFile

instance (Observes m) => Observes (Observing m) where
  observe observation = Observing \_ _ -> observe observation

hoist :: (MonadError Logic.Report m) => m a -> Observing m a
hoist ma = Observing \_ path -> Report.nesting path ma
