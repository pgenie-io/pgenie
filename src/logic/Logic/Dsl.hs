module Logic.Dsl
  ( run,
    Script,
    Stages (..),
  )
where

import AlgebraicPath qualified as Path
import Base.Prelude hiding (readFile, writeFile)
import Control.Monad.Parallel qualified as MonadParallel
import Data.Aeson.Text qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Logic.Algebra
import Logic.GeneratorHashes qualified as GeneratorHashes
import Logic.Name qualified as Name
import Logic.ProjectFile qualified as ProjectFile
import Logic.SqlTemplate qualified as SqlTemplate
import Logic.SyntaxAnalyser qualified as SyntaxAnalyser
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import PGenieGen.Model.Output.Report qualified as Gen.Output.Report
import SyntacticClass qualified as Syntactic

run :: (Caps m) => Script a -> m a
run (Script f) = f 1 []

-- |
-- Monad for defining pure logic scripts.
newtype Script a
  = Script (forall m. (Caps m) => Double -> [Text] -> m a)

instance Functor Script where
  fmap f (Script g) = Script \progress path ->
    fmap f (g progress path)

instance Applicative Script where
  pure a = Script \_ _ -> pure a
  Script f <*> Script a = Script \progress path ->
    f progress path <*> a progress path

instance Monad Script where
  Script ma >>= f = Script \progress path -> do
    a <- ma progress path
    let Script mb = f a
    mb progress path

instance MonadParallel.MonadParallel Script where
  bindM2 f (Script ma) (Script mb) = Script \progress path ->
    MonadParallel.bindM2
      (\a b -> let Script mc = f a b in mc progress path)
      (ma progress path)
      (mb progress path)

instance MonadError Error Script where
  throwError e = Script \_ path ->
    let newPath = path <> e.path
        newError = e {path = newPath}
     in throwError newError

  catchError (Script f) handler =
    Script \p q -> catchError (f p q) (\e -> let Script h = handler e in h p q)

instance Stages Script where
  stage name substagesCount (Script runInner) =
    Script \progressPerStage path -> do
      let newPath =
            if Text.null name
              then path
              else name : path
      emit (StageEntered newPath)
      (remainingProgress, result) <-
        if substagesCount > 0
          then (0,) <$> runInner (progressPerStage / fromIntegral substagesCount) newPath
          else (progressPerStage,) <$> runInner 0 newPath
      emit (StageExited newPath remainingProgress)
      pure result

instance DbOps Script where
  executeMigration migrationLoaded = liftWithErrs (executeMigration migrationLoaded)
  inferQueryTypes sqlTemplate = liftWithErrs (inferQueryTypes sqlTemplate)
  explainQuery sqlTemplate = liftWithErrs (explainQuery sqlTemplate)
  getIndexes = liftWithErrs getIndexes

instance FsOps Script where
  readFile path = liftWithErrs (readFile path)
  writeFile path content = liftWithErrs (writeFile path content)
  listDir path = liftWithErrs (listDir path)

instance LoadsGen Script where
  loadGen genLocation maybeHash = liftWithErrs (loadGen genLocation maybeHash)

instance Emits Script where
  emit event = Script \maxProgress path ->
    let nestStagePath stagePath = stagePath <> path
        nestError err =
          err {path = nestStagePath err.path}
        nestedEvent = case event of
          StageEntered stagePath ->
            StageEntered (nestStagePath stagePath)
          StageExited stagePath remainingProgress ->
            StageExited (nestStagePath stagePath) (remainingProgress * maxProgress)
          WarningEmitted err ->
            WarningEmitted (nestError err)
          Failed err ->
            Failed (nestError err)
     in emit nestedEvent

liftWithErrs :: (forall m. (Caps m) => m a) -> Script a
liftWithErrs ma = Script \_ path ->
  catchError ma \e ->
    let newPath = path <> e.path
        newError = e {path = newPath}
     in throwError newError

-- |
-- - Reports progress.
-- - Reports stage enter and exit for logging.
-- - Reports parallelism as @enters - exits@. Amount of actively running stages.
class (Monad m) => Stages m where
  -- | Wrap an action as a stage in progress.
  stage ::
    -- | Name of the stage. May be empty.
    Text ->
    -- | Amount of substages.
    --
    -- Each nested stage exit will increase the progress within this stage by @1 / amountOfSubstages@.
    --
    -- If there's no substages, pass @0@. Then only the exit of the whole stage will increase the progress.
    Int ->
    m a ->
    m a
