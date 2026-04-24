-- | Helper transformer that implements the 'Stages' and 'Warns' capabilities
-- for the infrastructure layer, mirroring what 'Logic.Dsl.Script' used to do
-- but living in the infra package.
module Infra.Adapters.Script
  ( Event (..),
    EmitsEvent (..),
    ScriptT (..),
    runScript,
  )
where

import Control.Monad.Parallel qualified as MonadParallel
import Data.Text qualified as Text
import Logic qualified
import Logic.Report qualified as Report
import Utils.Prelude hiding (readFile, writeFile)

-- * Event type

-- | Observation events produced during script execution.
data Event
  = StageEntered [Text]
  | StageExited [Text] Double
  | WarningEmitted Logic.Report
  | Failed Logic.Report
  deriving stock (Eq, Show)

-- * EmitsEvent capability

-- | Capability to emit infra-level events.  Implemented by the underlying
-- 'Fx' monad via the 'Device' environment so that 'ScriptT' can forward
-- stage and warning events without a direct IO dependency.
class (Monad m) => EmitsEvent m where
  emitEvent :: Event -> m ()

-- * ScriptT

-- | Transformer that tracks the current stage path and per-stage progress
-- budget, implementing the 'Logic.Stages' and 'Logic.Warns' capabilities.
newtype ScriptT m a = ScriptT {unScriptT :: Double -> [Text] -> m a}

-- | Run a 'ScriptT' action at the top level (full progress budget, empty path).
runScript :: ScriptT m a -> m a
runScript (ScriptT f) = f 1 []

-- * Instances

instance (Functor m) => Functor (ScriptT m) where
  fmap f (ScriptT g) = ScriptT \progress path ->
    fmap f (g progress path)

instance (Applicative m) => Applicative (ScriptT m) where
  pure a = ScriptT \_ _ -> pure a
  ScriptT f <*> ScriptT a = ScriptT \progress path ->
    f progress path <*> a progress path

instance (Monad m) => Monad (ScriptT m) where
  ScriptT ma >>= f = ScriptT \progress path -> do
    a <- ma progress path
    let ScriptT mb = f a
    mb progress path

instance (MonadParallel.MonadParallel m) => MonadParallel.MonadParallel (ScriptT m) where
  bindM2 f (ScriptT ma) (ScriptT mb) = ScriptT \progress path ->
    MonadParallel.bindM2
      (\a b -> let ScriptT mc = f a b in mc progress path)
      (ma progress path)
      (mb progress path)

instance (MonadError Logic.Report m) => MonadError Logic.Report (ScriptT m) where
  throwError e = ScriptT \_ path ->
    throwError (Report.nest path e)

  catchError (ScriptT f) handler =
    ScriptT \p q -> catchError (f p q) (\e -> let ScriptT h = handler e in h p q)

instance (EmitsEvent m, Monad m) => Logic.Stages (ScriptT m) where
  stage name substagesCount (ScriptT runInner) =
    ScriptT \progressPerStage path -> do
      let newPath =
            if Text.null name
              then path
              else name : path
      emitEvent (StageEntered newPath)
      (remainingProgress, result) <-
        if substagesCount > 0
          then (0,) <$> runInner (progressPerStage / fromIntegral substagesCount) newPath
          else (progressPerStage,) <$> runInner 0 newPath
      emitEvent (StageExited newPath remainingProgress)
      pure result

instance (EmitsEvent m, Monad m) => Logic.Warns (ScriptT m) where
  warn report = ScriptT \_ path -> emitEvent (WarningEmitted (Report.nest path report))

-- Lift capability instances from the underlying monad.

instance (Logic.DbOps m) => Logic.DbOps (ScriptT m) where
  executeMigration sql = liftScript (Logic.executeMigration sql)
  inferQueryTypes sql = liftScript (Logic.inferQueryTypes sql)
  explainQuery sql = liftScript (Logic.explainQuery sql)
  getIndexes = liftScript Logic.getIndexes

instance (Logic.FsOps m) => Logic.FsOps (ScriptT m) where
  readFile path = liftScript (Logic.readFile path)
  writeFile path content = liftScript (Logic.writeFile path content)
  listDir path = liftScript (Logic.listDir path)

instance (Logic.LoadsGen m) => Logic.LoadsGen (ScriptT m) where
  loadGen loc hash = liftScript (Logic.loadGen loc hash)

instance (Logic.LoadsProjectFile m) => Logic.LoadsProjectFile (ScriptT m) where
  loadProjectFile = ScriptT \_ _ -> Logic.loadProjectFile

instance (EmitsEvent m) => EmitsEvent (ScriptT m) where
  emitEvent event = ScriptT \_ _ -> emitEvent event

-- | Lift an action from @m@ into 'ScriptT m', nesting the current path into
-- any error it throws.
liftScript :: (MonadError Logic.Report m) => m a -> ScriptT m a
liftScript ma = ScriptT \_ path ->
  catchError ma \e ->
    throwError (Report.nest path e)
