module Logic.Capabilities.QueryAnalysis
  ( InfersQueryTypes (..),
  )
where

import Logic.Domain.QueryAnalysis (InferredQueryTypes)
import Logic.Domain.Report (Report)
import Utils.Prelude

class (MonadError Report m) => InfersQueryTypes m where
  inferQueryTypes :: Text -> m (InferredQueryTypes, [Report])
