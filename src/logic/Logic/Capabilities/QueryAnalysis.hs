-- |
-- Port for inferring the parameter and result types of a SQL query.
module Logic.Capabilities.QueryAnalysis
  ( InfersQueryTypes (..),
  )
where

import Logic.Domain.QueryAnalysis (InferredQueryTypes)
import Logic.Domain.Report (Report)
import Utils.Prelude

-- | Capability to infer the types of a query's parameters and result columns.
class (MonadError Report m) => InfersQueryTypes m where
  inferQueryTypes :: Text -> m (InferredQueryTypes, [Report])
