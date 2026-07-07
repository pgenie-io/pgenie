module Logic.Capabilities.Reporting
  ( Warns (..),
  )
where

import Logic.Domain.Report (Report)
import Utils.Prelude

class (Monad m) => Warns m where
  warn :: Report -> m ()
