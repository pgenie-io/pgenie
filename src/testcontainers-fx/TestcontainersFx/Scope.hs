-- |
-- Adapter for the `TestContainer` monad to `Scope`.
module TestcontainersFx.Scope where

import Base.Prelude
import Fx
import TestContainers qualified as Tc
import TestcontainersFx.Scope.IO qualified as IO

testContainer :: Tc.TestContainer a -> Scope SomeException a
testContainer startContainers =
  acquire (runExceptionalIO (const (IO.acquire startContainers)))
    & releasing (runExceptionalIO (IO.release . snd))
    & fmap fst
