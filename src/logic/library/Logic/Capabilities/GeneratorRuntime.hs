-- |
-- Port for loading a generator implementation from its declared location.
module Logic.Capabilities.GeneratorRuntime
  ( LoadsGen (..),
  )
where

import GenBridge qualified as Gen
import Utils.Prelude

-- | Capability to load a generator's code and integrity hash from a location.
class (Monad m) => LoadsGen m where
  loadGen ::
    Gen.Location ->
    Maybe Text ->
    m (Gen.Gen, Text)
