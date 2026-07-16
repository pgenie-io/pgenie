-- |
-- The fake latest rung of the throwaway mock chain, with a genuinely
-- fallible 'GenContractVersioning.downgradeInput' -- negative inputs are
-- rejected, mirroring how a real newer contract can express something an
-- older one has no way to encode.
module Mock.MockV3 (MockV3) where

import GenContractVersioning (ContractVersion (..), HasPreviousVersion (..), IsContractVersion (..), chainedCodecByVersion)
import Mock.MockV2 (MockV2)
import Prelude

data MockV3

instance IsContractVersion MockV3 where
  type InputOf MockV3 = Int
  type OutputOf MockV3 = Int

  versionOf = ContractVersion {major = 3, minor = 1}

  codecByVersion = chainedCodecByVersion @MockV3

instance HasPreviousVersion MockV3 where
  type PreviousVersionOf MockV3 = MockV2

  downgradeInput input
    | input < 0 = Left "MockV3: negative inputs cannot be represented in the older contract"
    | otherwise = Right (input - 100)

  upgradeOutput output = output + 1000
