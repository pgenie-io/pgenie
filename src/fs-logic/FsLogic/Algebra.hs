module FsLogic.Algebra where

import Base.Prelude hiding (readFile, writeFile)
import Test.QuickCheck.Arbitrary qualified as Arbitrary

-- * Domain

data Error
  = FileNotFoundError Path
  | FileReadError Path Text
  | FileWriteError Path Text
  | FileDeleteError Path Text
  | DirectoryNotFoundError Path
  | DirectoryReadError Path Text
  | DirectoryWriteError Path Text
  | DirectoryDeleteError Path Text
  deriving stock (Show, Eq)

data Path = Path
  deriving stock (Show, Eq)

data PathSegment = PathSegment
  deriving stock (Show, Eq)

-- ** Domain Instances

instance Arbitrary.Arbitrary Path where
  arbitrary =
    error "TODO"

instance Arbitrary.Arbitrary PathSegment where
  arbitrary =
    error "TODO"

instance IsSome Path PathSegment where
  to = error "TODO"
  maybeFrom = error "TODO"

instance Semigroup Path where
  Path <> Path =
    error "TODO"

instance Monoid Path where
  mempty =
    error "TODO"

-- * Operations

class (MonadError Error m) => ControlsFiles m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()
  deleteFile :: Path -> m ()
  checkFile :: Path -> m Bool
  listDir :: Path -> m [Path]
  checkDir :: Path -> m Bool
  createDir :: Path -> m ()
  deleteDir :: Path -> m ()

overwriteFile :: (ControlsFiles m) => Path -> Text -> m Bool
overwriteFile path content = do
  exists <- checkFile path
  if exists
    then do
      deleteFile path
      writeFile path content
      pure True
    else do
      writeFile path content
      pure False
