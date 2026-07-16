-- |
-- A fake middle rung of the throwaway mock chain: has a previous rung
-- ('Mock.MockV1.MockV1'), with a total, always-succeeding projection in
-- each direction.
module Mock.MockV2 (MockV2) where

import GenContractVersioning (ContractVersion (..), HasPreviousVersion (..), IsContractVersion (..), chainedCodecByVersion)
import Mock.MockV1 (MockV1)
import Prelude

data MockV2

instance IsContractVersion MockV2 where
  type InputOf MockV2 = Int
  type OutputOf MockV2 = Int

  versionOf = ContractVersion {major = 2, minor = 0}

  codecByVersion = chainedCodecByVersion @MockV2

instance HasPreviousVersion MockV2 where
  type PreviousVersionOf MockV2 = MockV1

  -- \| Marks the hop by subtracting a distinct offset, so a test can tell
  -- how many hops an input crossed by how far it moved from its original
  -- value.
  downgradeInput input = Right (input - 10)

  upgradeOutput output = output + 100
