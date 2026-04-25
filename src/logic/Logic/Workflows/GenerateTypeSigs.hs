module Logic.Workflows.GenerateTypeSigs where

import Logic.Features.CustomTypeSignatureFile qualified as CustomTypeSignatureFile
import Logic.Features.Fs (FsOps)
import Logic.Features.Report (Report)
import Logic.Features.Staging (Stages (..))
import PGenieGen.Model.Input qualified as Gen.Input
import Utils.Prelude

type Port m = (Stages m, FsOps m, MonadError Report m)

run :: (Port m) => [Gen.Input.CustomType] -> m [Gen.Input.CustomType]
run customTypes =
  stage "Custom types" (length customTypes) do
    for customTypes \ct ->
      stage (ct.pgSchema <> "." <> ct.pgName) 0 do
        CustomTypeSignatureFile.refineFromSignatureFile ct
