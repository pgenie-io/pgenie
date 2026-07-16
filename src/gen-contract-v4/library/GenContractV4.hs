-- | Ties gen-contract v4's Input/Output types to the 'V4' tag from
-- this module. V4 is the oldest supported rung: it is its own
-- 'GenContractVersioning.PreviousVersionOf' fixed point, per
-- "GenContractVersioning"'s module haddock.
module GenContractV4 (V4) where

import GenContractV4.Contract (Output, Project)
import GenContractVersioning (ContractVersion (..), IsContractVersion (..), codecByVersionDefault)
import Utils.Prelude

-- | gen-contract v4 (the oldest currently-supported rung - its own
-- 'GenContractVersioning.PreviousVersionOf' fixed point).
data V4

instance IsContractVersion V4 where
  type InputOf V4 = Project
  type OutputOf V4 = Output
  type PreviousVersionOf V4 = V4

  versionOf = ContractVersion {major = 4, minor = 0}

  downgradeInput = Right
  upgradeOutput = id

  codecByVersion = codecByVersionDefault @V4
