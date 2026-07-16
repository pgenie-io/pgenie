-- |
-- A fake, throwaway oldest rung, used only to pin the chain-walking
-- dispatcher's behaviour against a 2+-rung chain without depending on any
-- real gen-contract package.
module Mock.MockV1 (MockV1) where

import GenContractVersioning (ContractVersion (..), IsContractVersion (..), codecByVersionDefault)
import Prelude

-- | The oldest fake rung -- its own 'GenContractVersioning.PreviousVersionOf' fixed point.
data MockV1

instance IsContractVersion MockV1 where
  type InputOf MockV1 = Int
  type OutputOf MockV1 = Int
  type PreviousVersionOf MockV1 = MockV1

  versionOf = ContractVersion {major = 1, minor = 0}

  downgradeInput = Right
  upgradeOutput = id

  codecByVersion = codecByVersionDefault @MockV1
