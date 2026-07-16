-- |
-- A fake, throwaway oldest rung, used only to pin the chain-walking
-- dispatcher's behaviour against a 2+-rung chain without depending on any
-- real gen-contract package.
module Mock.MockV1 (MockV1) where

import GenContractVersioning (ContractVersion (..), IsContractVersion (..), terminalCodecByVersion)
import Prelude

-- | The oldest fake rung -- no 'GenContractVersioning.HasPreviousVersion' instance.
data MockV1

instance IsContractVersion MockV1 where
  type InputOf MockV1 = Int
  type OutputOf MockV1 = Int

  versionOf = ContractVersion {major = 1, minor = 0}

  codecByVersion = terminalCodecByVersion @MockV1
