module Logic.Features.QuerySignatures.Workflows.GenerateQuerySignatures where

import AlgebraicPath qualified as Path
import Logic.Features.Fs.Port (FsOps (..))
import Logic.Features.QuerySignatures.Types.QuerySignatures qualified as SignatureFile
import Logic.Features.Reporting.Port (Warns (..))
import Logic.Features.Reporting.Types.Report (Report (..))
import PGenieGen.Model.Input qualified as Gen.Input
import Utils.Prelude hiding (readFile, writeFile)

type Port m = (MonadError Report m, Warns m, FsOps m)

data Params = Params
  { sigPath :: Path,
    inferredSig :: SignatureFile.Signature,
    inferredParams :: [Gen.Input.Member],
    inferredResult :: Maybe Gen.Input.ResultRows
  }

data Result = Result
  { params :: [Gen.Input.Member],
    result :: Maybe Gen.Input.ResultRows,
    idempotent :: Bool
  }

run :: (Port m) => Params -> m Result
run p = do
  maybeSigContent <-
    catchError
      (Just <$> readFile p.sigPath)
      (\(_ :: Report) -> pure Nothing)
  case maybeSigContent of
    Nothing -> do
      writeFile p.sigPath (SignatureFile.serialize p.inferredSig)
      pure
        Result
          { params = p.inferredParams,
            result = p.inferredResult,
            idempotent = p.inferredSig.idempotent
          }
    Just sigContent -> do
      fileSig <- case SignatureFile.tryParse sigContent of
        Left err ->
          throwError
            ( Report
                []
                "Failed to parse signature file"
                (Just "Check the YAML syntax in the signature file")
                [("file", Path.toText p.sigPath), ("error", err)]
            )
        Right sig -> pure sig
      case SignatureFile.validateAndMerge p.inferredSig fileSig of
        Left err -> throwError err
        Right mergedSig ->
          let (mergedParams, mergedResult) =
                SignatureFile.applyToQuery mergedSig p.inferredParams p.inferredResult
           in pure
                Result
                  { params = mergedParams,
                    result = mergedResult,
                    idempotent = mergedSig.idempotent
                  }
