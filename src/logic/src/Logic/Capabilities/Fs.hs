-- |
-- Port for reading, writing, and listing filesystem paths.
module Logic.Capabilities.Fs
  ( FsOps (..),
  )
where

import Utils.Prelude hiding (readFile, writeFile)

class (Monad m) => FsOps m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()
  listDir :: Path -> m [Path]
