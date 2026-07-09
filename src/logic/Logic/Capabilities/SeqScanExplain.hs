-- |
-- Port for obtaining a query's PostgreSQL @EXPLAIN@ output.
module Logic.Capabilities.SeqScanExplain
  ( ExplainsQuery (..),
  )
where

import Logic.Domain.Report (Report)
import Utils.Prelude

-- | Capability to run @EXPLAIN@ on a query and return its plan as text lines.
class (MonadError Report m) => ExplainsQuery m where
  explainQuery :: Text -> m [Text]
