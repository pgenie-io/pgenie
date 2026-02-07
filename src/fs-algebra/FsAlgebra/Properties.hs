module FsAlgebra.Properties where

import Base.Prelude hiding (readFile, writeFile)
import FsAlgebra.Algebra
import Test.QuickCheck
import Test.QuickCheck.Monadic

readingAWrittenFileProducesTheSameContent ::
  forall m.
  (ControlsFiles m) =>
  (forall a. m a -> IO a) ->
  Property
readingAWrittenFileProducesTheSameContent execute = monadicIO do
  path <- pick arbitrary
  content <- pick arbitrary
  result <- run $ execute $ do
    writeFile path content
    readFile path
  pure (result === content)

createdDirectoriesGetListed ::
  forall m.
  (ControlsFiles m) =>
  (forall a. m a -> IO a) ->
  Property
createdDirectoriesGetListed execute = do
  error "TODO"

listedDirectoriesExist ::
  forall m.
  (ControlsFiles m) =>
  (forall a. m a -> IO a) ->
  Property
listedDirectoriesExist execute = do
  error "TODO"
