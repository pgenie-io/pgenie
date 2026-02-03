module ReportingLogic.Algebra
  ( -- * Ports
    Reports (..),
  )
where

import Base.Prelude
import Control.Arrow

-- |
-- - Reports progress.
-- - Reports stage enter and exit for logging.
-- - Reports parallelism as @enters - exits@. Amount of actively running stages.
class (Monad m) => Reports m where
  -- | Wrap an action as a stage in progress.
  stage ::
    (Reports m) =>
    -- | Name of the stage. May be empty.
    Text ->
    -- | Amount of substages.
    --
    -- Each nested stage exit will increase the progress within this stage by @1 / amountOfSubstages@.
    Int ->
    m a ->
    m a
