module ParallelismLogic.Algebra
  ( Parallelism (..),
    Parallelly,
    parallelly,
    runParallelly,
    runApPar,
  )
where

import Base.Prelude

class (Monad m) => Parallelism m where
  liftA2Par :: (a -> b -> c) -> m a -> m b -> m c

newtype Parallelly m a = Parallelly (m a)

instance (Functor m) => Functor (Parallelly m) where
  fmap f (Parallelly m) = Parallelly (fmap f m)

instance (Parallelism m) => Applicative (Parallelly m) where
  pure x = Parallelly (pure x)
  Parallelly f <*> Parallelly g = Parallelly (liftA2Par id f g)

parallelly :: m a -> Parallelly m a
parallelly = Parallelly

-- | Unlift a 'Parallelly' action to the underlying monad.
runParallelly :: (Parallelism m) => Parallelly m a -> m a
runParallelly (Parallelly m) = m

runApPar ::
  (Parallelism m) =>
  (forall f. (Applicative f) => (forall a. m a -> f a) -> f a) ->
  m a
runApPar f =
  runParallelly (f parallelly)
