module Logic.Features.GeneratorRuntime.Port
  ( LoadsGen (..),
  )
where

import PGenieGen qualified as Gen
import Utils.Prelude

class (Monad m) => LoadsGen m where
  loadGen ::
    Gen.Location ->
    Maybe Text ->
    m (Gen.Gen, Text)
