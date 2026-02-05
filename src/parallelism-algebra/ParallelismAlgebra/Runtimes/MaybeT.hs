{-# OPTIONS_GHC -Wno-orphans #-}

module ParallelismAlgebra.Runtimes.MaybeT where

import Base.Prelude
import ParallelismAlgebra.Algebra

instance (Parallelism m) => Parallelism (MaybeT m) where
  apPar (MaybeT mf) (MaybeT ma) =
    MaybeT (apPar (fmap (<*>) mf) ma)
