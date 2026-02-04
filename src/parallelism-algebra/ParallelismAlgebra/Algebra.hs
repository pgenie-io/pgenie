module ParallelismAlgebra.Algebra
  ( Parallelism (..),
    Parallelly,
    parallelly,
    runParallelly,
  )
where

import Base.Prelude

class (Monad m) => Parallelism m where
  apPar :: m (a -> b) -> m a -> m b
  apPar = ap

newtype Parallelly m a = Parallelly (m a)

instance (Functor m) => Functor (Parallelly m) where
  fmap f (Parallelly m) = Parallelly (fmap f m)

instance (Parallelism m) => Applicative (Parallelly m) where
  pure x = Parallelly (pure x)
  Parallelly f <*> Parallelly g = Parallelly (apPar f g)

parallelly :: m a -> Parallelly m a
parallelly = Parallelly

-- | Unlift a 'Parallelly' action to the underlying monad.
runParallelly :: (Parallelism m) => Parallelly m a -> m a
runParallelly (Parallelly m) = m
