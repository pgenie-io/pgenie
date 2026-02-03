{-# OPTIONS_GHC -Wno-orphans #-}

module ParallelismLogic.Adapters.ExceptT where

import Base.Prelude
import ParallelismLogic.Algebra

instance (Parallelism m) => Parallelism (ExceptT e m) where
  apPar (ExceptT mf) (ExceptT ma) =
    ExceptT (apPar (fmap (<*>) mf) ma)
