module Logic.Features.CustomTypeSignatures.Workflows.GenerateTypeSignatures where

import Logic.Features.CustomTypeSignatures.Types.CustomTypeSignatures qualified as CustomTypeSignatureFile
import Logic.Features.Fs.Port (FsOps)
import Logic.Features.Reporting.Types.Report (Report)
import Logic.Features.Staging.Port (Stages (..))
import PGenieGen.Model.Input qualified as Gen.Input
import Utils.Prelude

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
          CustomTypeSignatureFile.refineFromSignatureFile ct
    pure Result {refinedCustomTypes}
