{-# OPTIONS_GHC -Wno-orphans #-}

module ParallelismAlgebra.Adapters.ExceptT where

import Base.Prelude
import ParallelismAlgebra.Algebra

instance (Parallelism m) => Parallelism (ExceptT e m) where
  apPar (ExceptT mf) (ExceptT ma) =
    ExceptT (apPar (fmap (<*>) mf) ma)
