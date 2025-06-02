module ProgressLogic.Algebras.Reports where

import Base.Prelude

-- |
-- - Reports progress.
-- - Reports stage enter and exit for logging.
-- - Reports parallellism as @enters - exits@. Amount of actively running stages.
class (Monad m) => Reports m where
  reportStageEnter :: Text -> m ()
  reportStageExit ::
    Text ->
    -- | Overall progress of the scope, from 0 to 1.
    Double ->
    m ()
  mapReports :: (Double -> Double) -> m a -> m a
