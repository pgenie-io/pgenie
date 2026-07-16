-- |
-- The classy algebra behind pgenie's gen-contract backcompat mechanism
-- (issue #76): each supported major contract version has an associated
-- phantom tag type (defined in its own contract package); 'IsContractVersion'
-- names its Input/Output types, declares the highest contract version this
-- build implements for it, names its immediately-older rung
-- ('PreviousVersionOf'), how to project a value across that one hop in each
-- direction ('downgradeInput'\/'upgradeOutput'), and dispatches a requested
-- version to a 'Codec' relative to this rung's types.
--
-- The oldest rung this build supports is its own 'PreviousVersionOf': a
-- self-referential fixed point (@type PreviousVersionOf c = c@,
-- @downgradeInput = Right@, @upgradeOutput = id@, all written explicitly, no
-- defaults) rather than a separate class for "has no older rung." This
-- trades a compile-time fact (a rung is terminal iff it has no
-- @HasPreviousVersion@ instance) for a runtime one (a rung is terminal iff
-- @versionOf \@c == versionOf \@(PreviousVersionOf c)@) — see 'codecByVersion'
-- and #78's design discussion for the full trade-off. The mitigation: every
-- new rung's test suite must prove a hop-through actually transforms values
-- (not identity), and the then-oldest rung's test must prove the fixed
-- point rejects an older major with 'UnsupportedMajorDispatchError'.
--
-- The chain walk itself lives here as 'codecByVersionDefault': every rung's
-- 'codecByVersion' delegates to it with a one-line method body, so adding a
-- major is new contract package + instances + repointing the newest rung's
-- caller, not a hand-written dispatch branch.
module GenContractVersioning
  ( ContractVersion (..),
    IsContractVersion (..),
    Codec (..),
    DispatchError (..),
    dispatchErrorToText,
    codecByVersionDefault,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Dhall qualified
import GenContractVersioning.Codec (Codec (..), identityCodec)
import GenContractVersioning.ContractVersion (ContractVersion (..))
import GenContractVersioning.DispatchError (DispatchError (..))
import GenContractVersioning.DispatchError qualified as DispatchError
import Prelude

-- | Names the Input/Output Haskell types for one gen-contract major version
-- tag, reflects the version it implements, names its immediately-older rung,
-- and how to project a value across that one hop in each direction.
--
-- The oldest supported rung is its own 'PreviousVersionOf' (see the module
-- haddock); there is no separate marker for "has no older rung," so every
-- instance — terminal or not — writes all members explicitly.
class IsContractVersion c where
  type InputOf c :: Type
  type OutputOf c :: Type
  type PreviousVersionOf c :: Type

  -- | The highest @(major, minor)@ contract version this build implements
  -- for tag @c@. A loaded generator declaring this major is accepted iff
  -- its declared minor does not exceed this one (see
  -- 'GenContractVersioning.ContractVersion.ContractVersion' for the
  -- rationale). Ambiguous by design: apply as @versionOf \@c@.
  versionOf :: ContractVersion

  -- | Project the latest-rung input down to its previous rung. Can fail:
  -- going from a newer contract to an older one can genuinely lose
  -- representability (a feature the newer contract can express that the
  -- older one has no way to encode). The oldest rung defines this as
  -- @Right@ (an identity at its own fixed point).
  downgradeInput :: InputOf c -> Either Text (InputOf (PreviousVersionOf c))

  -- | Lift the previous rung's output up to this rung. Always total: an
  -- older generator's output is always representable in a newer contract
  -- (backcompat only ever adds capability, never removes it). The oldest
  -- rung defines this as @id@ (an identity at its own fixed point).
  upgradeOutput :: OutputOf (PreviousVersionOf c) -> OutputOf c

  -- | Given a generator's declared version, either reject it or hand back
  -- a 'Codec' between this rung's own Input/Output types and the Dhall
  -- expression the loader operates on. Every instance implements this with
  -- a one-line delegation to 'codecByVersionDefault'.
  codecByVersion :: ContractVersion -> Either DispatchError (Codec (InputOf c) (OutputOf c))

-- | The chain-walk default every 'IsContractVersion' instance's
-- 'codecByVersion' delegates to. Matches iff the requested major equals
-- this rung's, and the requested minor does not exceed it (never falls
-- through to an older rung's types on a minor mismatch, only a major one).
-- On a major miss: if this rung is its own 'PreviousVersionOf' (the oldest
-- supported rung, per the module haddock's fixed-point convention), the
-- requested major is unsupported outright; otherwise recurses into the
-- previous rung's 'codecByVersion' and wraps its 'Codec' with
-- 'downgradeInput'\/'upgradeOutput' to re-relativize it to this rung's
-- types.
codecByVersionDefault ::
  forall c.
  (IsContractVersion c, IsContractVersion (PreviousVersionOf c), Dhall.ToDhall (InputOf c), Dhall.FromDhall (OutputOf c)) =>
  ContractVersion ->
  Either DispatchError (Codec (InputOf c) (OutputOf c))
codecByVersionDefault requested
  | requested.major == implemented.major =
      if requested.minor > implemented.minor
        then Left (IncompatibleMinorDispatchError {requestedMinor = requested.minor, implementedMinor = implemented.minor})
        else Right identityCodec
  | atFixedPoint =
      Left (UnsupportedMajorDispatchError {requestedMajor = requested.major, oldestSupportedMajor = implemented.major})
  | otherwise = do
      previous <- codecByVersion @(PreviousVersionOf c) requested
      Right
        Codec
          { encode = \input -> downgradeInput @c input >>= previous.encode,
            decode = fmap (upgradeOutput @c) . previous.decode
          }
  where
    implemented = versionOf @c
    atFixedPoint = implemented == versionOf @(PreviousVersionOf c)

-- | Render a 'DispatchError' as the diagnostic text surfaced to generator
-- authors.
dispatchErrorToText :: DispatchError -> Text
dispatchErrorToText = DispatchError.toText
