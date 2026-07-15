-- |
-- Port for reading the current state of database indexes.
module Logic.Capabilities.IndexCatalog
  ( LoadsIndexes (..),
  )
where

import Logic.Domain.IndexOptimization (IndexInfo)
import Logic.Domain.Report (Report)
import Utils.Prelude

-- | Capability to retrieve the set of indexes currently present in the database.
class (MonadError Report m) => LoadsIndexes m where
  getIndexes :: m [IndexInfo]
