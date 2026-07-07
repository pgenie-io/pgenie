module Logic.Capabilities.IndexCatalog
  ( LoadsIndexes (..),
  )
where

import Logic.Domain.IndexOptimization (IndexInfo)
import Logic.Domain.Report (Report)
import Utils.Prelude

class (MonadError Report m) => LoadsIndexes m where
  getIndexes :: m [IndexInfo]
