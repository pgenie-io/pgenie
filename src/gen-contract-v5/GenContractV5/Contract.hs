-- |
-- Ties gen-contract v5's Input/Output types to the 'V5' tag from
-- this module, and gives the real projection to/from v4: the
-- exact field-level diff between gen-contract v4.0.1 and v5.0.0 is (1)
-- @Scalar.Custom@ carries a full 'GenContractV5.Input.CustomTypeRef'
-- instead of a bare 'Name' -- downgrading drops @pgSchema@/@pgName@/
-- @index@, keeping only @name@; (2) 'GenContractV5.Input.Value' inlines
-- @dimensionality@/@elementIsNullable@ directly instead of wrapping them
-- in an @Optional ArraySettings@ -- downgrading re-wraps them, using
-- 'Nothing' when @dimensionality == 0@. Both directions are total for
-- this rung (neither ever needs the 'Left' that 'downgradeInput''s
-- signature allows for future, genuinely-lossy rungs).
module GenContractV5.Contract
  ( V5,
    toV4Project,
    toV4CustomType,
    toV4CustomTypeDefinition,
    toV4Member,
    toV4Value,
    toV4Scalar,
    toV4Query,
    toV4Result,
    toV4ResultRows,
    toV5Output,
    toV5Report,
    toV5File,
  )
where

import GenContractV4.Contract (V4)
import GenContractV5.Input.CustomType (toV4CustomType)
import GenContractV5.Input.CustomTypeDefinition (toV4CustomTypeDefinition)
import GenContractV5.Input.Member (toV4Member)
import GenContractV5.Input.Project (Project, toV4Project)
import GenContractV5.Input.Query (toV4Query)
import GenContractV5.Input.Result (toV4Result)
import GenContractV5.Input.ResultRows (toV4ResultRows)
import GenContractV5.Input.Scalar (toV4Scalar)
import GenContractV5.Input.Value (toV4Value)
import GenContractV5.Output.File (toV5File)
import GenContractV5.Output.Output (Output, toV5Output)
import GenContractV5.Output.Report (toV5Report)
import GenContractVersioning (HasParent (..), IsContract (..))
import Utils.Prelude

-- | gen-contract v5 (the current latest rung).
data V5

instance IsContract V5 where
  type InputOf V5 = Project
  type OutputOf V5 = Output

instance HasParent V5 where
  type ParentOf V5 = V4

  downgradeInput = Right . toV4Project

  upgradeOutput = toV5Output
