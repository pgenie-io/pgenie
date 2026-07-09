module Logic.Procedures.GenerateTypeSignatures where

import AlgebraicPath qualified as Path
import GenBridge.Model.Input qualified as Gen.Input
import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.Staging (Stages (..))
import Logic.Domain.CustomTypeSignature qualified as CustomTypeSignatureFile
import Logic.Domain.Report (Report (..))
import Utils.Prelude hiding (readFile, writeFile)

type Port m = (Stages m, FsOps m, MonadError Report m)

data Params = Params
  { customTypes :: [Gen.Input.CustomType]
  }

data Result = Result
  { refinedCustomTypes :: [Gen.Input.CustomType]
  }

run :: (Port m) => Params -> m Result
run Params {customTypes} =
  stage "Custom types" (length customTypes) do
    refinedCustomTypes <-
      for customTypes \ct ->
        stage (ct.pgSchema <> "." <> ct.pgName) 0 do
          case CustomTypeSignatureFile.fromInferred ct of
            Nothing -> pure ct
            Just inferredSig -> do
              let sigPath = CustomTypeSignatureFile.customTypeSignatureFilePath ct.pgSchema ct.pgName
              maybeSigContent <-
                catchError
                  (Just <$> readFile sigPath)
                  (\(_ :: Report) -> pure Nothing)
              case maybeSigContent of
                Nothing -> do
                  writeFile sigPath (CustomTypeSignatureFile.serialize inferredSig)
                  pure ct
                Just sigContent -> do
                  fileSig <- case CustomTypeSignatureFile.tryParse sigContent of
                    Left err ->
                      throwError
                        ( Report
                            []
                            "Failed to parse custom-type signature file"
                            (Just "Check the YAML syntax in the signature file")
                            [("file", Path.toText sigPath), ("error", err)]
                        )
                    Right sig -> pure sig
                  case CustomTypeSignatureFile.validateAndMerge ct fileSig of
                    Left err -> throwError err
                    Right refined -> pure refined
    pure Result {refinedCustomTypes}
