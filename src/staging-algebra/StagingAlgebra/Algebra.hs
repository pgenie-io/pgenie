module StagingAlgebra.Algebra
  ( -- * Ports
    Stages (..),
  )
where

import Base.Prelude

-- |
-- - Reports progress.
-- - Reports stage enter and exit for logging.
-- - Reports parallelism as @enters - exits@. Amount of actively running stages.
class (Monad m) => Stages m where
  -- | Wrap an action as a stage in progress.
  stage ::
    -- | Name of the stage. May be empty.
    Text ->
    -- | Amount of substages.
    --
    -- Each nested stage exit will increase the progress within this stage by @1 / amountOfSubstages@.
    --
    -- If there's no substages, pass @0@. Then only the exit of the whole stage will increase the progress.
    Int ->
    m a ->
    m a
