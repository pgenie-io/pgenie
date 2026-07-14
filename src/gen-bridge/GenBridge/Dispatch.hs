-- |
-- The hand-written major-contract-version dispatcher: given a loaded
-- generator's declared @(major, minor)@, decides which gen-contract rung it
-- targets, checks that rung's minor ceiling, and hands back adapters that
-- let the rest of GenBridge treat every supported rung as the latest v5
-- 'Input'/'Output'.
module GenBridge.Dispatch
  ( Adapters (..),
    dispatch,
  )
where

import Dhall qualified
import GenBridge.ContractVersion qualified as ContractVersion
import GenContractV5.Input qualified as V5.Input
import GenContractV5.Input.Project qualified as V5.Project
import GenContractV5.Output qualified as V5.Output
import GenContractV5.Output.Output qualified as V5.Output
import Utils.Prelude

-- | Adapters for one concrete contract major.
data Adapters input output = Adapters
  { projectInput :: V5.Input.Project -> Either Text input,
    liftOutput :: output -> V5.Output.Output
  }

-- | Dispatch on a generator's declared @(major, minor)@.
dispatch ::
  Natural ->
  Natural ->
  (forall input output. (Dhall.ToDhall input, Dhall.FromDhall output) => Adapters input output -> Either Text a) ->
  Either Text a
dispatch major minor k
  | major == 5 =
      if minor > ContractVersion.current.minor
        then
          Left
            ( "Incompatible contract minor version: "
                <> onto (show minor)
                <> ". Expected "
                <> onto (show ContractVersion.current.minor)
                <> " or lower."
            )
        else k Adapters {projectInput = Right, liftOutput = id}
  | major == 4 =
      if minor > 0
        then Left ("Incompatible contract minor version: " <> onto (show minor) <> ". Expected 0 or lower.")
        else k Adapters {projectInput = Right . V5.Project.toV4Project, liftOutput = V5.Output.fromV4Output}
  | otherwise =
      Left ("Unsupported contract major version: " <> onto (show major) <> ". Oldest supported is 4.")
