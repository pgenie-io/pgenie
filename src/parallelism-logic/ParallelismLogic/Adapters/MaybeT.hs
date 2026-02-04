{-# OPTIONS_GHC -Wno-orphans #-}

module ParallelismLogic.Adapters.MaybeT where

import Base.Prelude
import ParallelismLogic.Algebra

instance (Parallelism m) => Parallelism (MaybeT m) where
  apPar (MaybeT mf) (MaybeT ma) =
    MaybeT (apPar (fmap (<*>) mf) ma)
