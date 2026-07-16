-- |
-- Ties gen-contract v5's Input/Output types to the 'V5' tag from
-- this module, and gives the real projection to/from v4: the
-- exact field-level diff between gen-contract v4.0.1 and v5.0.0 is (1)
-- @Scalar.Custom@ carries a full 'GenContractV5.Contract.CustomTypeRef'
-- instead of a bare 'Name' -- downgrading drops @pgSchema@/@pgName@/
-- @index@, keeping only @name@; (2) 'GenContractV5.Contract.Value' inlines
-- @dimensionality@/@elementIsNullable@ directly instead of wrapping them
-- in an @Optional ArraySettings@ -- downgrading re-wraps them, using
-- 'Nothing' when @dimensionality == 0@. Output is untouched between the
-- two rungs, so 'upgradeOutput' is 'id'. Both input directions are total
-- for this rung (neither ever needs the 'Left' that 'downgradeInput''s
-- signature allows for future, genuinely-lossy rungs).
module GenContractV5
  ( V5,
  )
where

import GenContractV4 (V4)
import GenContractV5.Contract (Output)
import GenContractV5.Contract.Project (Project, toV4Project)
import GenContractVersioning (ContractVersion (..), HasPreviousVersion (..), IsContractVersion (..), chainedCodecByVersion)
import Utils.Prelude

-- | gen-contract v5 (the current latest rung).
data V5

instance IsContractVersion V5 where
  type InputOf V5 = Project
  type OutputOf V5 = Output

  versionOf = ContractVersion {major = 5, minor = 0}

  codecByVersion = chainedCodecByVersion @V5

instance HasPreviousVersion V5 where
  type PreviousVersionOf V5 = V4

  downgradeInput = Right . toV4Project

  upgradeOutput = id
