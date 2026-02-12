-- |
-- Adapter for the `TestContainer` monad to `Scope`.
module Infra.Adapters.Analyser.Scopes.Testcontainers where

import Base.Prelude
import Fx
import Infra.Adapters.Analyser.Scopes.Testcontainers.IO qualified as IO
import TestContainers qualified as Tc

testContainer :: Tc.TestContainer a -> Scope SomeException a
testContainer startContainers = do
  (a, internalState) <- acquire (runExceptionalIO (const (IO.acquire startContainers)))

  registerRelease (runExceptionalIO (const (IO.release internalState)))

  pure a
