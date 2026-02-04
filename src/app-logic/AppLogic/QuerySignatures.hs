module AppLogic.QuerySignatures where

import Base.Prelude hiding (writeFile)
import ParallelismAlgebra qualified as Parallelism
import StagingAlgebra.Algebra qualified as ReportingLogic

-- * Model

-- | Migrations error.
data Error

data QuerySig

data QueryName

-- * Effect

class
  ( MonadError Error m,
    Parallelism.Parallelism m,
    ReportingLogic.Reports m
  ) =>
  ControlsQuerySignatures m
  where
  -- | Attempt to load the query signature file.
  --
  -- Missing file is not an error. Parsing failure of an existing file however is.
  loadQuerySignature :: Path -> QueryName -> m QuerySig

  -- | Create or replace the signature file for the query.
  generateSignature :: Path -> QuerySig -> m ()

-- * Logic

enrichQuerySig :: (ControlsQuerySignatures m) => QuerySig -> QuerySig -> m QuerySig
enrichQuerySig =
  error "TODO"
