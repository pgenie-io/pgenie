{-# OPTIONS_GHC -Wno-orphans #-}

module ParallelismLogic.Adapters.ReaderT where

import Base.Prelude
import ParallelismLogic.Algebra

instance (Parallelism m) => Parallelism (ReaderT e m) where
  apPar (ReaderT mf) (ReaderT ma) =
    ReaderT (\env -> apPar (mf env) (ma env))
