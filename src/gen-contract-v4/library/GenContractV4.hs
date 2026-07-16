-- | Ties gen-contract v4's Input/Output types to the 'V4' tag from
-- this module. No 'GenContractVersioning.HasPreviousVersion' instance -
-- v4 is the oldest supported rung.
module GenContractV4 (V4) where

import GenContractV4.Contract (Output, Project)
import GenContractVersioning (ContractVersion (..), IsContractVersion (..))

-- | gen-contract v4 (the oldest currently-supported rung - no 'HasPreviousVersion' instance exists for it).
data V4

instance IsContractVersion V4 where
  type InputOf V4 = Project
  type OutputOf V4 = Output

  versionOf = ContractVersion {major = 4, minor = 0}
