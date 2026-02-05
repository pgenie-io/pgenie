{-# OPTIONS_GHC -Wno-orphans #-}

module ParallelismAlgebra.Runtimes.IO where

import Base.Prelude
import ParallelismAlgebra.Algebra

instance Parallelism IO where
  apPar action1 action2 = mask \unmask -> do
    leftResultVar <- newEmptyMVar
    forkIOWithUnmask \unmask -> do
      a <- unmask (try action1)
      putMVar leftResultVar a
    b <- unmask (try action2)
    a <- unmask (try (takeMVar leftResultVar))
    case join a <*> b of
      Left err -> throwIO @SomeException err
      Right res -> pure res
