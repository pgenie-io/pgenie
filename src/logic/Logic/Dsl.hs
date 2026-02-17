module Logic.Dsl where

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

runLogic :: (Caps m) => Logic a -> m a
runLogic (Logic f) = f 1 []

-- |
-- Monad for defining pure logic scripts.
newtype Logic a
  = Logic (forall m. (Caps m) => Double -> [Text] -> m a)

instance Functor Logic where
  fmap f (Logic g) = Logic \progress path ->
    fmap f (g progress path)

instance Applicative Logic where
  pure a = Logic \_ _ -> pure a
  Logic f <*> Logic a = Logic \progress path ->
    f progress path <*> a progress path

instance Monad Logic where
  Logic ma >>= f = Logic \progress path -> do
    a <- ma progress path
    let Logic mb = f a
    mb progress path

instance MonadParallel.MonadParallel Logic where
  bindM2 f (Logic ma) (Logic mb) = Logic \progress path ->
    MonadParallel.bindM2
      (\a b -> let Logic mc = f a b in mc progress path)
      (ma progress path)
      (mb progress path)

instance MonadError Error Logic where
  throwError e = Logic \_ path ->
    let newPath = path <> e.path
        newError = e {path = newPath}
     in throwError newError

  catchError (Logic f) handler =
    Logic \p q -> catchError (f p q) (\e -> let Logic h = handler e in h p q)

instance Stages Logic where
  stage name substagesCount (Logic runInner) =
    Logic \progressPerStage path -> do
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

instance DbOps Logic where
  executeMigration migrationLoaded = liftWithErrs (executeMigration migrationLoaded)
  inferQueryTypes sqlTemplate = liftWithErrs (inferQueryTypes sqlTemplate)

instance FsOps Logic where
  readFile path = liftWithErrs (readFile path)
  writeFile path content = liftWithErrs (writeFile path content)
  listDir path = liftWithErrs (listDir path)

instance LoadsGen Logic where
  loadGen genLocation maybeHash = liftWithErrs (loadGen genLocation maybeHash)

instance Emits Logic where
  emit event = Logic \_ _ -> emit event

liftWithErrs :: (forall m. (Caps m) => m a) -> Logic a
liftWithErrs ma = Logic \_ path ->
  catchError ma \e ->
    let newPath = path <> e.path
        newError = e {path = newPath}
     in throwError newError
