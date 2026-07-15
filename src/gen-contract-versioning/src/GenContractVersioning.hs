{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- The classy algebra behind pgenie's gen-contract backcompat mechanism
-- (issue #76): each supported major contract version has an associated
-- phantom tag type (defined in its own contract sublibrary);
-- 'IsContract' names its Input/Output types;
-- 'HasParent' says it has an immediately-older rung and how to project
-- to/from it. Deliberately carries no version-number reflection (e.g. a
-- @majorVersion@ method) -- these classes describe pure structural mapping
-- only. The dispatcher that decides which tag a generator's declared
-- major maps to, and walks multiple 'HasParent' hops when needed, is
-- hand-written in @GenBridge.Dispatch@, not derived generically here.
module GenContractVersioning
  ( IsContract (..),
    HasParent (..),
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Prelude

-- | Names the Input/Output Haskell types for one gen-contract major
-- version tag.
class IsContract c where
  type InputOf c :: Type
  type OutputOf c :: Type

-- | Says tag @c@ has an immediately-older rung, 'ParentOf' @c@, and how to
-- project a value across that one hop in each direction.
class (IsContract c) => HasParent c where
  type ParentOf c :: Type

  -- | Project the latest-rung input down to its parent rung. Can fail:
  -- going from a newer contract to an older one can genuinely lose
  -- representability (a feature the newer contract can express that the
  -- older one has no way to encode).
  downgradeInput :: InputOf c -> Either Text (InputOf (ParentOf c))

  -- | Lift the parent rung's output up to this rung. Always total: an
  -- older generator's output is always representable in a newer contract
  -- (backcompat only ever adds capability, never removes it).
  upgradeOutput :: OutputOf (ParentOf c) -> OutputOf c
