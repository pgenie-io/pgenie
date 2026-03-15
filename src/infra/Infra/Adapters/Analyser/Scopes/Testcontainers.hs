-- |
-- Adapter for the `TestContainer` monad to `Scope`.
module Infra.Adapters.Analyser.Scopes.Testcontainers where

import Fx
import Infra.Adapters.Analyser.Scopes.Testcontainers.IO qualified as IO
import TestContainers qualified as Tc
import Utils.Prelude

testContainer :: Tc.TestContainer a -> Scope SomeException a
testContainer startContainers = do
  (a, internalState) <- acquire (runExceptionalIO (const (IO.acquire startContainers)))

  registerRelease (runExceptionalIO (const (IO.release internalState)))

  pure a
