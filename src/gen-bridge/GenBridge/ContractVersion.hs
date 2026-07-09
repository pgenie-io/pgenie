module GenBridge.ContractVersion where

import Dhall qualified
import GenBridge.Dhall.Orphans ()
import Utils.Prelude

data ContractVersion = ContractVersion
  { major :: Natural,
    minor :: Natural
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

current :: ContractVersion
current = ContractVersion {major = 4, minor = 0}
