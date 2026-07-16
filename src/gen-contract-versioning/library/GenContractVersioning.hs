-- |
-- The classy algebra behind pgenie's gen-contract backcompat mechanism
-- (issue #76): each supported major contract version has an associated
-- phantom tag type (defined in its own contract package);
-- 'IsContractVersion' names its Input/Output types, declares the highest
-- contract version this build implements for it, and dispatches a
-- requested version to a 'Codec' relative to this rung's types;
-- 'HasPreviousVersion' says it has an immediately-older rung and how to
-- project to/from it.
--
-- The chain walk itself (issue #78) lives here as 'terminalCodecByVersion'
-- and 'chainedCodecByVersion': every rung's 'codecByVersion' delegates to
-- one of these with a one-line method body, so adding a major is new
-- contract package + instances + repointing the newest rung's caller, not a
-- hand-written dispatch branch.
module GenContractVersioning
  ( ContractVersion (..),
    IsContractVersion (..),
    HasPreviousVersion (..),
    Codec (..),
    DispatchError (..),
    dispatchErrorToText,
    terminalCodecByVersion,
    chainedCodecByVersion,
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

  -- | Given a generator's declared version, either reject it or hand back
  -- a 'Codec' between this rung's own Input/Output types and the Dhall
  -- expression the loader operates on. Every instance implements this with
  -- a one-line delegation to 'terminalCodecByVersion' (no
  -- 'HasPreviousVersion' instance) or 'chainedCodecByVersion' (has one).
  codecByVersion :: ContractVersion -> Either DispatchError (Codec (InputOf c) (OutputOf c))

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

-- | 'codecByVersion' for a rung with no 'HasPreviousVersion' instance: the
-- oldest rung this build supports. Matches iff the requested major equals
-- this rung's, and the requested minor does not exceed it; otherwise the
-- requested major is unsupported outright, since there is no older rung
-- left to try.
terminalCodecByVersion ::
  forall c.
  (IsContractVersion c, Dhall.ToDhall (InputOf c), Dhall.FromDhall (OutputOf c)) =>
  ContractVersion ->
  Either DispatchError (Codec (InputOf c) (OutputOf c))
terminalCodecByVersion requested =
  matchOwnRung @c requested (Left (UnsupportedMajorDispatchError {requestedMajor = requested.major, oldestSupportedMajor = (versionOf @c).major}))

-- | 'codecByVersion' for a rung with a 'HasPreviousVersion' instance.
-- Matches against this rung the same way 'terminalCodecByVersion' does; on
-- a major miss, recurses into the previous rung's 'codecByVersion' and
-- wraps its 'Codec' with 'downgradeInput'\/'upgradeOutput' to re-relativize
-- it to this rung's types.
chainedCodecByVersion ::
  forall c.
  (HasPreviousVersion c, IsContractVersion (PreviousVersionOf c), Dhall.ToDhall (InputOf c), Dhall.FromDhall (OutputOf c)) =>
  ContractVersion ->
  Either DispatchError (Codec (InputOf c) (OutputOf c))
chainedCodecByVersion requested =
  matchOwnRung @c requested do
    previous <- codecByVersion @(PreviousVersionOf c) requested
    Right
      Codec
        { encode = \input -> downgradeInput @c input >>= previous.encode,
          decode = fmap (upgradeOutput @c) . previous.decode
        }

-- | Shared match/fail-through logic between 'terminalCodecByVersion' and
-- 'chainedCodecByVersion': accept iff majors agree and the requested minor
-- does not exceed this rung's; if majors agree but the minor is too high,
-- fail definitively (never fall through to an older rung with different
-- types); if majors disagree, run @onMiss@ (recurse, or report
-- unsupported).
matchOwnRung ::
  forall c.
  (IsContractVersion c, Dhall.ToDhall (InputOf c), Dhall.FromDhall (OutputOf c)) =>
  ContractVersion ->
  Either DispatchError (Codec (InputOf c) (OutputOf c)) ->
  Either DispatchError (Codec (InputOf c) (OutputOf c))
matchOwnRung requested onMiss
  | requested.major /= implemented.major = onMiss
  | requested.minor > implemented.minor =
      Left (IncompatibleMinorDispatchError {requestedMinor = requested.minor, implementedMinor = implemented.minor})
  | otherwise = Right identityCodec
  where
    implemented = versionOf @c

-- | Render a 'DispatchError' as the diagnostic text surfaced to generator
-- authors.
dispatchErrorToText :: DispatchError -> Text
dispatchErrorToText = DispatchError.toText
