{-# OPTIONS_GHC -Wno-orphans #-}

module ParallelismLogic.Adapters.IO where

import Base.Prelude
import ParallelismLogic.Algebra

instance Parallelism IO where
  liftA2Par f action1 action2 = do
    leftResultVar <- newEmptyMVar
    forkIO do
      a <- action1
      putMVar leftResultVar a
    b <- action2
    a <- takeMVar leftResultVar
    pure (f a b)
