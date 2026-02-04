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
createdDirectoriesGetListed execute = monadicIO do
  containerPath <- to <$> pick (arbitrary @PathSegment)
  paths <- fmap (mappend containerPath . to) <$> pick (arbitrary @[PathSegment])
  result <- run $ execute $ do
    createDir containerPath
    forM_ paths $ \path -> do
      createDir path
    fmap (mappend containerPath) <$> listDir containerPath
  pure (all (flip elem result) paths)

listedDirectoriesExist ::
  forall m.
  (ControlsFiles m) =>
  (forall a. m a -> IO a) ->
  Property
listedDirectoriesExist execute = monadicIO do
  containerPath <- to <$> pick (arbitrary @PathSegment)
  paths <- fmap (mappend containerPath . to) <$> pick (arbitrary @[PathSegment])
  result <- run $ execute $ do
    createDir containerPath
    forM_ paths $ \path -> do
      createDir path
    paths <- fmap (mappend containerPath) <$> listDir containerPath
    forM paths $ \path -> do
      exists <- isDirectory path
      pure (path, exists)
  let missingPaths =
        result
          & filter (not . snd)
          & fmap fst
  pure (missingPaths === [])
