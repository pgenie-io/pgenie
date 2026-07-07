module Logic.Capabilities.SeqScanExplain
  ( ExplainsQuery (..),
  )
where

import Logic.Domain.Report (Report)
import Utils.Prelude

class (MonadError Report m) => ExplainsQuery m where
  explainQuery :: Text -> m [Text]
