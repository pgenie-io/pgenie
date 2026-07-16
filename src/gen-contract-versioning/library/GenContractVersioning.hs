-- |
-- The classy algebra behind pgenie's gen-contract backcompat mechanism
-- (issue #76): each supported major contract version has an associated
-- phantom tag type (defined in its own contract package);
-- 'IsContractVersion' names its Input/Output types and declares the
-- highest contract version this build implements for it;
-- 'HasPreviousVersion' says it has an immediately-older rung and how to
-- project to/from it. The dispatcher that matches a generator's declared
-- version against a tag and walks multiple 'HasPreviousVersion' hops when
-- needed is hand-written in @GenBridge.Dispatch@, not derived generically
-- here.
module GenContractVersioning
  ( ContractVersion (..),
    IsContractVersion (..),
    HasPreviousVersion (..),
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import GenContractVersioning.ContractVersion (ContractVersion (..))
import Prelude

-- | Names the Input/Output Haskell types for one gen-contract major
-- version tag and reflects the version it implements.
class IsContractVersion c where
  type InputOf c :: Type
  type OutputOf c :: Type

  -- | The highest @(major, minor)@ contract version this build implements
  -- for tag @c@. A loaded generator declaring this major is accepted iff
  -- its declared minor does not exceed this one (see
  -- 'GenContractVersioning.ContractVersion.ContractVersion' for the
  -- rationale). Ambiguous by design: apply as @versionOf \@c@.
  versionOf :: ContractVersion

-- | Says tag @c@ has an immediately-older rung, 'PreviousVersionOf' @c@,
-- and how to project a value across that one hop in each direction.
class (IsContractVersion c) => HasPreviousVersion c where
  type PreviousVersionOf c :: Type

  -- | Project the latest-rung input down to its previous rung. Can fail:
  -- going from a newer contract to an older one can genuinely lose
  -- representability (a feature the newer contract can express that the
  -- older one has no way to encode).
  downgradeInput :: InputOf c -> Either Text (InputOf (PreviousVersionOf c))

  -- | Lift the previous rung's output up to this rung. Always total: an
  -- older generator's output is always representable in a newer contract
  -- (backcompat only ever adds capability, never removes it).
  upgradeOutput :: OutputOf (PreviousVersionOf c) -> OutputOf c
