-- | Pure runtime interpreter for staged logic that emits structured
-- observations for a host environment to render or record.
module Runtime.Emitting
  ( EmitsObservation (..),
    Emitting (..),
    interpretEmitting,
  )
where

import Control.Monad.Parallel qualified as MonadParallel
import Data.Text qualified as Text
import Logic qualified
import Logic.Report qualified as Report
import Runtime.Observation qualified as Observation
import Utils.Prelude hiding (readFile, writeFile)

-- | Capability to publish runtime observations.
class (Monad m) => EmitsObservation m where
  emitObservation :: Observation.Observation -> m ()

-- | Transformer that tracks the current stage path and per-stage progress
-- budget, implementing the 'Logic.Stages' and 'Logic.Warns' capabilities.
newtype Emitting m a = Emitting {unEmitting :: Double -> [Text] -> m a}

-- | Interpret an 'Emitting' action at the top level with full progress budget.
interpretEmitting :: Emitting m a -> m a
interpretEmitting (Emitting f) = f 1 []

instance (Functor m) => Functor (Emitting m) where
  fmap f (Emitting g) = Emitting \progress path ->
    fmap f (g progress path)

instance (Applicative m) => Applicative (Emitting m) where
  pure a = Emitting \_ _ -> pure a
  Emitting f <*> Emitting a = Emitting \progress path ->
    f progress path <*> a progress path

instance (Monad m) => Monad (Emitting m) where
  Emitting ma >>= f = Emitting \progress path -> do
    a <- ma progress path
    let Emitting mb = f a
    mb progress path

instance (MonadParallel.MonadParallel m) => MonadParallel.MonadParallel (Emitting m) where
  bindM2 f (Emitting ma) (Emitting mb) = Emitting \progress path ->
    MonadParallel.bindM2
      (\a b -> let Emitting mc = f a b in mc progress path)
      (ma progress path)
      (mb progress path)

instance (MonadError Logic.Report m) => MonadError Logic.Report (Emitting m) where
  throwError e = Emitting \_ path ->
    throwError (Report.nest path e)

  catchError (Emitting f) handler =
    Emitting \progress path -> catchError (f progress path) (\e -> let Emitting h = handler e in h progress path)

instance (EmitsObservation m, Monad m) => Logic.Stages (Emitting m) where
  stage name substagesCount (Emitting runInner) =
    Emitting \progressPerStage path -> do
      let newPath =
            if Text.null name
              then path
              else name : path
      emitObservation (Observation.StageEntered newPath)
      (remainingProgress, result) <-
        if substagesCount > 0
          then (0,) <$> runInner (progressPerStage / fromIntegral substagesCount) newPath
          else (progressPerStage,) <$> runInner 0 newPath
      emitObservation (Observation.StageExited newPath remainingProgress)
      pure result

instance (EmitsObservation m, Monad m) => Logic.Warns (Emitting m) where
  warn report = Emitting \_ path -> emitObservation (Observation.WarningEmitted (Report.nest path report))

instance (Logic.DbOps m) => Logic.DbOps (Emitting m) where
  executeMigration sql = hoist (Logic.executeMigration sql)
  inferQueryTypes sql = hoist (Logic.inferQueryTypes sql)
  explainQuery sql = hoist (Logic.explainQuery sql)
  getIndexes = hoist Logic.getIndexes

instance (Logic.FsOps m) => Logic.FsOps (Emitting m) where
  readFile path = hoist (Logic.readFile path)
  writeFile path content = hoist (Logic.writeFile path content)
  listDir path = hoist (Logic.listDir path)

instance (Logic.LoadsGen m) => Logic.LoadsGen (Emitting m) where
  loadGen loc hash = hoist (Logic.loadGen loc hash)

instance (Logic.LoadsProjectFile m) => Logic.LoadsProjectFile (Emitting m) where
  loadProjectFile = Emitting \_ _ -> Logic.loadProjectFile

instance (EmitsObservation m) => EmitsObservation (Emitting m) where
  emitObservation observation = Emitting \_ _ -> emitObservation observation

hoist :: (MonadError Logic.Report m) => m a -> Emitting m a
hoist ma = Emitting \_ path -> Report.nesting path ma
