module Logic.Features.IndexCatalog.Port
  ( LoadsIndexes (..),
  )
where

import Logic.Features.IndexOptimization.Types.IndexOptimization (IndexInfo)
import Logic.Features.Reporting.Types.Report (Report)
import Utils.Prelude

class (MonadError Report m) => LoadsIndexes m where
  getIndexes :: m [IndexInfo]
