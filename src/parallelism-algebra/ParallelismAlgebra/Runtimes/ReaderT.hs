{-# OPTIONS_GHC -Wno-orphans #-}

module ParallelismAlgebra.Runtimes.ReaderT where

import Base.Prelude
import ParallelismAlgebra.Algebra

instance (Parallelism m) => Parallelism (ReaderT e m) where
  apPar (ReaderT mf) (ReaderT ma) =
    ReaderT (\env -> apPar (mf env) (ma env))
