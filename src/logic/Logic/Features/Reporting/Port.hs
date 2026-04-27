module Logic.Features.Reporting.Port
  ( Warns (..),
  )
where

import Logic.Features.Reporting.Types.Report (Report)
import Utils.Prelude

class (Monad m) => Warns m where
  warn :: Report -> m ()
