-- |
-- Port for surfacing non-fatal problem reports to the user.
module Logic.Capabilities.Reporting
  ( Warns (..),
  )
where

import Logic.Domain.Report (Report)
import Utils.Prelude

-- | Capability to emit a warning without aborting the running procedure.
class (Monad m) => Warns m where
  warn :: Report -> m ()
