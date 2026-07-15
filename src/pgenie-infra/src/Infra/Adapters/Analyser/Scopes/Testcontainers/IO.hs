-- |
-- Raw 'IO' acquire/release pair for a 'Tc.TestContainer' setup, factored out
-- of "Infra.Adapters.Analyser.Scopes.Testcontainers" so the @Scope@ adapter
-- above it stays free of resourcet's internal-state plumbing.
module Infra.Adapters.Analyser.Scopes.Testcontainers.IO where

import Control.Monad.Trans.Resource qualified as ResourceT
import Control.Monad.Trans.Resource.Internal qualified as ResourceT
import Data.Acquire qualified as ResourceT
import TestContainers qualified as Tc
import TestContainers.Monad qualified as Tc
import Utils.Prelude

-- | Determine the Testcontainers config and run the setup, capturing the
-- resourcet internal state needed to release its resources later.
acquire :: Tc.TestContainer a -> IO (a, ResourceT.InternalState)
acquire startContainers = do
  config <- Tc.determineConfig
  Tc.runTestContainer config do
    result <- startContainers
    releaseMap <- ResourceT.liftResourceT ResourceT.getInternalState
    liftIO (ResourceT.stateAlloc releaseMap)
    pure (result, releaseMap)

-- | Release the resources captured by a prior 'acquire' call.
release :: ResourceT.InternalState -> IO ()
release releaseMap =
  ResourceT.stateCleanup ResourceT.ReleaseNormal releaseMap
