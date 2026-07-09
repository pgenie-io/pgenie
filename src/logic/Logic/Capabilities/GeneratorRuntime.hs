module Logic.Capabilities.GeneratorRuntime
  ( LoadsGen (..),
  )
where

import GenBridge qualified as Gen
import Utils.Prelude

class (Monad m) => LoadsGen m where
  loadGen ::
    Gen.Location ->
    Maybe Text ->
    m (Gen.Gen, Text)
