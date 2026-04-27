module Logic.Features.SeqScanExplain.Port
  ( ExplainsQuery (..),
  )
where

import Logic.Features.Reporting.Types.Report (Report)
import Utils.Prelude

class (MonadError Report m) => ExplainsQuery m where
  explainQuery :: Text -> m [Text]
