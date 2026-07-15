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
import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.GeneratorRuntime (LoadsGen (..))
import Logic.Capabilities.IndexCatalog (LoadsIndexes (..))
import Logic.Capabilities.Migrations (ExecutesMigrations (..))
import Logic.Capabilities.QueryAnalysis (InfersQueryTypes (..))
import Logic.Capabilities.Reporting (Warns (..))
import Logic.Capabilities.SeqScanExplain (ExplainsQuery (..))
import Logic.Capabilities.Staging (Stages (..))
import Logic.Domain.Report qualified as Report
import Utils.Prelude hiding (readFile, writeFile)

-- | Observations produced while executing capability-based logic.
data Observation
  = -- | Execution entered the named stage path.
    StageEntered [Text]
  | -- | Execution left the named stage path, with the progress budget
    -- remaining unused by that stage redistributed to the caller.
    StageExited [Text] Double
  | -- | A non-fatal warning was raised at the given stage path.
    WarningEmitted Report.Report
  | -- | Execution aborted at the given stage path.
    ExecutionFailed Report.Report
  deriving stock (Eq, Show)

-- | Capability to publish runtime observations.
class (Monad m) => Observes m where
  -- | Publish an observation to the host environment for rendering or
  -- recording.
  observe :: Observation -> m ()

-- | Transformer that tracks the current stage path and per-stage progress
-- budget, implementing the 'Stages' and 'Warns' capabilities.
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

instance (MonadError Report.Report m) => MonadError Report.Report (Observing m) where
  throwError e = Observing \_ path ->
    throwError (Report.nest path e)

  catchError (Observing f) handler =
    Observing \progress path -> catchError (f progress path) (\e -> let Observing h = handler e in h progress path)

instance (Observes m, Monad m) => Stages (Observing m) where
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

instance (Observes m, Monad m) => Warns (Observing m) where
  warn report = Observing \_ path -> observe (WarningEmitted (Report.nest path report))

instance (ExecutesMigrations m) => ExecutesMigrations (Observing m) where
  executeMigration sql = lift (executeMigration sql)

instance (InfersQueryTypes m) => InfersQueryTypes (Observing m) where
  inferQueryTypes sql = lift (inferQueryTypes sql)

instance (ExplainsQuery m) => ExplainsQuery (Observing m) where
  explainQuery sql = lift (explainQuery sql)

instance (LoadsIndexes m) => LoadsIndexes (Observing m) where
  getIndexes = lift getIndexes

instance (FsOps m) => FsOps (Observing m) where
  readFile path = lift (readFile path)
  writeFile path content = lift (writeFile path content)
  listDir path = lift (listDir path)

instance (LoadsGen m) => LoadsGen (Observing m) where
  loadGen loc hash = lift (loadGen loc hash)

instance (Observes m) => Observes (Observing m) where
  observe observation = lift (observe observation)

instance MonadTrans Observing where
  lift ma = Observing \_ _ -> ma
