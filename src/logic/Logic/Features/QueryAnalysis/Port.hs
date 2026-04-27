module Logic.Features.QueryAnalysis.Port
  ( InfersQueryTypes (..),
  )
where

import Logic.Features.QueryAnalysis.Types.QueryAnalysis (InferredQueryTypes)
import Logic.Features.Reporting.Types.Report (Report)
import Utils.Prelude

class (MonadError Report m) => InfersQueryTypes m where
  inferQueryTypes :: Text -> m (InferredQueryTypes, [Report])
