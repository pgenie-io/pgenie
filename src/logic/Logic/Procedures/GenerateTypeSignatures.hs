-- |
-- Persists and reconciles each custom type's inferred field signature
-- against its on-disk custom-type signature file, so hand-authored
-- overrides (e.g. field nullability) survive re-generation.
module Logic.Procedures.GenerateTypeSignatures
  ( Port,
    Params (..),
    Result (..),
    run,
  )
where

import AlgebraicPath qualified as Path
import GenBridge.Contract qualified as Gen.Input
import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.Staging (Stages (..))
import Logic.Domain.CustomTypeSignature qualified as CustomTypeSignature
import Logic.Domain.Report (Report (..))
import Utils.Prelude hiding (readFile, writeFile)

-- | Everything this procedure needs from its execution context.
type Port m = (Stages m, FsOps m, MonadError Report m)

-- | Input to the custom-type signature reconciliation procedure.
data Params = Params
  { customTypes :: [Gen.Input.CustomType]
  }

-- | Output: the custom types to use, each merged with any existing
-- signature file.
data Result = Result
  { refinedCustomTypes :: [Gen.Input.CustomType]
  }

-- | For each custom type: write a new signature file if none exists yet;
-- otherwise merge the freshly inferred signature with the existing file
-- and use the merged result, failing if the file is malformed or the
-- merge is inconsistent.
run :: (Port m) => Params -> m Result
run Params {customTypes} =
  stage "Custom types" (length customTypes) do
    refinedCustomTypes <-
      for customTypes \ct ->
        stage (ct.pgSchema <> "." <> ct.pgName) 0 do
          case CustomTypeSignature.fromInferred ct of
            Nothing -> pure ct
            Just inferredSig -> do
              let sigPath = CustomTypeSignature.customTypeSignatureFilePath ct.pgSchema ct.pgName
              maybeSigContent <-
                catchError
                  (Just <$> readFile sigPath)
                  (\(_ :: Report) -> pure Nothing)
              case maybeSigContent of
                Nothing -> do
                  writeFile sigPath (CustomTypeSignature.serialize inferredSig)
                  pure ct
                Just sigContent -> do
                  fileSig <- case CustomTypeSignature.tryParse sigContent of
                    Left err ->
                      throwError
                        ( Report
                            []
                            "Failed to parse custom-type signature file"
                            (Just "Check the YAML syntax in the signature file")
                            [("file", Path.toText sigPath), ("error", err)]
                        )
                    Right sig -> pure sig
                  case CustomTypeSignature.validateAndMerge ct fileSig of
                    Left err -> throwError err
                    Right refined -> pure refined
    pure Result {refinedCustomTypes}
