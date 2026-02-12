module Infra.Adapters.Analyser.Scopes.Testcontainers.IO where

import Base.Prelude
import Control.Monad.Trans.Resource qualified as ResourceT
import Control.Monad.Trans.Resource.Internal qualified as ResourceT
import Data.Acquire qualified as ResourceT
import TestContainers qualified as Tc
import TestContainers.Monad qualified as Tc

acquire :: Tc.TestContainer a -> IO (a, ResourceT.InternalState)
acquire startContainers = do
  config <- Tc.determineConfig
  Tc.runTestContainer config do
    result <- startContainers
    releaseMap <- ResourceT.liftResourceT ResourceT.getInternalState
    liftIO (ResourceT.stateAlloc releaseMap)
    pure (result, releaseMap)

release :: ResourceT.InternalState -> IO ()
release releaseMap =
  ResourceT.stateCleanup ResourceT.ReleaseNormal releaseMap
