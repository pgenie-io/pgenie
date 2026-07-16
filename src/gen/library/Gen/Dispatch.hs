-- |
-- The hand-written major-contract-version dispatcher: given a loaded
-- generator's declared @(major, minor)@, decides which gen-contract rung it
-- targets, checks that rung's minor ceiling, and hands back adapters that
-- let the rest of Gen treat every supported rung as the latest v5
-- 'Input'/'Output'. Carries no version literals of its own: every number
-- comes from the rungs' 'GenContractVersioning.versionOf' declarations.
module Gen.Dispatch
  ( Adapters (..),
    dispatch,
  )
where

import Dhall qualified
import GenContractV5 (V5)
import GenContractV5.Contract qualified as V5
import GenContractVersioning (ContractVersion (..), HasPreviousVersion (..), IsContractVersion (..))
import Utils.Prelude

-- | Adapters for one concrete contract major.
data Adapters input output = Adapters
  { projectInput :: V5.Project -> Either Text input,
    liftOutput :: output -> V5.Output
  }

-- | Dispatch on a generator's declared @(major, minor)@.
dispatch ::
  Natural ->
  Natural ->
  (forall input output. (Dhall.ToDhall input, Dhall.FromDhall output) => Adapters input output -> Either Text a) ->
  Either Text a
dispatch major minor k
  | major == (versionOf @V5).major = do
      checkMinorCeiling (versionOf @V5)
      k Adapters {projectInput = Right, liftOutput = id}
  | major == (versionOf @(PreviousVersionOf V5)).major = do
      checkMinorCeiling (versionOf @(PreviousVersionOf V5))
      k Adapters {projectInput = downgradeInput @V5, liftOutput = upgradeOutput @V5}
  | otherwise =
      Left
        ( "Unsupported contract major version: "
            <> onto (show major)
            <> ". Oldest supported is "
            <> onto (show (versionOf @(PreviousVersionOf V5)).major)
            <> "."
        )
  where
    checkMinorCeiling implemented =
      if minor > implemented.minor
        then
          Left
            ( "Incompatible contract minor version: "
                <> onto (show minor)
                <> ". Expected "
                <> onto (show implemented.minor)
                <> " or lower."
            )
        else Right ()
