-- | Ties gen-contract v4's Input/Output types to the 'V4' tag from
-- this module. No 'GenContractVersioning.HasParent' instance -
-- v4 is the oldest supported rung.
module GenContractV4.Contract (V4) where

import GenContractV4.Input (Project)
import GenContractV4.Output (Output)
import GenContractVersioning (IsContract (..))

-- | gen-contract v4 (the oldest currently-supported rung - no 'HasParent' instance exists for it).
data V4

instance IsContract V4 where
  type InputOf V4 = Project
  type OutputOf V4 = Output
