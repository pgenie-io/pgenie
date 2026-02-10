{-# OPTIONS_GHC -Wno-orphans #-}

module ParallelismAlgebra.Runtimes.Fx where

import Base.Prelude
import Fx
import ParallelismAlgebra.Algebra

instance Parallelism (Fx env err) where
  apPar action1 action2 =
    concurrently \lift ->
      liftA2 ($) (lift action1) (lift action2)
