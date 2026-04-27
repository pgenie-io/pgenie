module Logic.Features.Fs.Port
  ( FsOps (..),
  )
where

import Utils.Prelude hiding (readFile, writeFile)

class (Monad m) => FsOps m where
  readFile :: Path -> m Text
  writeFile :: Path -> Text -> m ()
  listDir :: Path -> m [Path]
