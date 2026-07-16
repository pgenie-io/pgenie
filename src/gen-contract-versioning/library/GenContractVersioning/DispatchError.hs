-- |
-- Why a generator's declared contract version could not be dispatched to
-- any rung the chain walk visited.
module GenContractVersioning.DispatchError
  ( DispatchError (..),
    toText,
  )
where

import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)
import Prelude

-- | Carries enough detail to explain the miss to a generator author,
-- distinguishing "no rung implements this major at all" from "a rung
-- implements the major, but this build's minor ceiling is lower" -- the two
-- failure modes the hand-written dispatcher used to report before the walk
-- was generalized.
data DispatchError
  = -- | No rung in the chain implements the requested major.
    UnsupportedMajorDispatchError
      { requestedMajor :: Natural,
        oldestSupportedMajor :: Natural
      }
  | -- | A rung implements the requested major, but its declared minor
    -- exceeds what this build implements for it.
    IncompatibleMinorDispatchError
      { requestedMinor :: Natural,
        implementedMinor :: Natural
      }
  deriving stock (Show, Eq)

-- | Render a 'DispatchError' as the diagnostic text surfaced to generator
-- authors.
toText :: DispatchError -> Text
toText = \case
  UnsupportedMajorDispatchError {requestedMajor, oldestSupportedMajor} ->
    "Unsupported contract major version: "
      <> Text.pack (show requestedMajor)
      <> ". Oldest supported is "
      <> Text.pack (show oldestSupportedMajor)
      <> "."
  IncompatibleMinorDispatchError {requestedMinor, implementedMinor} ->
    "Incompatible contract minor version: "
      <> Text.pack (show requestedMinor)
      <> ". Expected "
      <> Text.pack (show implementedMinor)
      <> " or lower."
