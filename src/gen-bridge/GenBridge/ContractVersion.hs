-- |
-- The Dhall generator contract's own version, checked against the
-- @contractVersion@ a loaded generator declares to reject incompatible
-- generators before running them.
module GenBridge.ContractVersion
  ( ContractVersion (..),
    current,
  )
where

import Dhall qualified
import GenBridge.Dhall.Orphans ()
import Utils.Prelude

-- | Semantic version of the generator contract (the shape a Dhall generator
-- file must expose: @contractVersion@, @Config@, @compile@).
data ContractVersion = ContractVersion
  { major :: Natural,
    minor :: Natural
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | The contract version this build of pgenie implements. A loaded
-- generator is rejected if its major version differs, or its minor version
-- is newer than this one (see "GenBridge.Load" and "GenBridge.Bundle").
current :: ContractVersion
current = ContractVersion {major = 5, minor = 0}
