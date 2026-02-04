module App.Effects.EmittingEvents where

import Base.Prelude
import StagingAlgebra

newtype EmittingEvents m a
  = EmittingEvents (Double -> m a)
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Double m)

instance (Reports m) => Stages (EmittingEvents m) where
  stage name substagesCount =
    if substagesCount > 0
      then \(EmittingEvents runInner) -> EmittingEvents \progressPerStage -> do
        enterStage name
        let progressPerSubstage = progressPerStage / fromIntegral substagesCount
        result <- runInner progressPerSubstage
        exitStage name 0
        pure result
      else \(EmittingEvents runInner) -> EmittingEvents \progressPerStage -> do
        enterStage name
        result <- runInner 0
        exitStage name progressPerStage
        pure result

class (Monad m) => Reports m where
  enterStage :: Text -> m ()
  exitStage :: Text -> Double -> m ()

run :: EmittingEvents m a -> m a
run (EmittingEvents runInner) = runInner 1.0
