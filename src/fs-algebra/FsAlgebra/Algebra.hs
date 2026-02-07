module FsAlgebra.Algebra where

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

data PathStatus
  = MissingPathStatus
  | IsFilePathStatus
  | IsDirectoryPathStatus
  deriving stock (Show, Eq)

-- * Operations

class (Monad m) => ControlsFiles m where
  readFile :: Path -> m ByteString
  writeFile :: Path -> ByteString -> m ()
  deleteFile :: Path -> m ()
  listDir :: Path -> m [Path]
  createDir :: Path -> m ()
  deleteDir :: Path -> m ()
  check :: Path -> m PathStatus

isFile :: (ControlsFiles m) => Path -> m Bool
isFile path =
  check path <&> \case
    IsFilePathStatus -> True
    _ -> False

isDirectory :: (ControlsFiles m) => Path -> m Bool
isDirectory path =
  check path <&> \case
    IsDirectoryPathStatus -> True
    _ -> False

overwriteFile :: (ControlsFiles m) => Path -> ByteString -> m Bool
overwriteFile path content = do
  status <- check path
  case status of
    IsFilePathStatus -> do
      deleteFile path
      writeFile path content
      pure True
    _ -> do
      writeFile path content
      pure False
