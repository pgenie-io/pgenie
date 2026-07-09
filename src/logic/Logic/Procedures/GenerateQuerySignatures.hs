-- |
-- Persists and reconciles a query's inferred parameter/result signature
-- against its on-disk @.signature@ file, so that changes to hand-authored
-- signature overrides survive re-generation.
module Logic.Procedures.GenerateQuerySignatures
  ( Port,
    Params (..),
    Result (..),
    run,
  )
where

import AlgebraicPath qualified as Path
import GenBridge.Model.Input qualified as Gen.Input
import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.Reporting (Warns (..))
import Logic.Domain.QuerySignature qualified as QuerySignature
import Logic.Domain.Report (Report (..))
import Utils.Prelude hiding (readFile, writeFile)

-- | Everything this procedure needs from its execution context.
type Port m = (MonadError Report m, Warns m, FsOps m)

-- | Input to the query signature reconciliation procedure.
data Params = Params
  { sigPath :: Path,
    inferredSig :: QuerySignature.Signature,
    inferredParams :: [Gen.Input.Member],
    inferredResult :: Gen.Input.Result
  }

-- | Output: the parameters and result to use (merged with any existing
-- signature file), and whether the query is idempotent.
data Result = Result
  { params :: [Gen.Input.Member],
    result :: Gen.Input.Result,
    idempotent :: Bool
  }

-- | Write a new signature file if none exists yet; otherwise merge the
-- freshly inferred signature with the existing file and use the merged
-- result, failing if the file is malformed or the merge is inconsistent.
run :: (Port m) => Params -> m Result
run p = do
  maybeSigContent <-
    catchError
      (Just <$> readFile p.sigPath)
      (\(_ :: Report) -> pure Nothing)
  case maybeSigContent of
    Nothing -> do
      writeFile p.sigPath (QuerySignature.serialize p.inferredSig)
      pure
        Result
          { params = p.inferredParams,
            result = p.inferredResult,
            idempotent = p.inferredSig.idempotent
          }
    Just sigContent -> do
      fileSig <- case QuerySignature.tryParse sigContent of
        Left err ->
          throwError
            ( Report
                []
                "Failed to parse signature file"
                (Just "Check the YAML syntax in the signature file")
                [("file", Path.toText p.sigPath), ("error", err)]
            )
        Right sig -> pure sig
      case QuerySignature.validateAndMerge p.inferredSig fileSig of
        Left err -> throwError err
        Right mergedSig ->
          let (mergedParams, mergedResult) =
                QuerySignature.applyToQuery mergedSig p.inferredParams p.inferredResult
           in pure
                Result
                  { params = mergedParams,
                    result = mergedResult,
                    idempotent = mergedSig.idempotent
                  }
