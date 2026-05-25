module Logic.Features.GeneratorRuntime.Workflows.GenerateCode where

import AlgebraicPath qualified as Path
import Control.Monad.Parallel qualified as MonadParallel
import Data.Aeson.Text qualified as Aeson.Text
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Logic.Features.Fs.Port (FsOps (..))
import Logic.Features.GeneratorRuntime.Port (LoadsGen (..))
import Logic.Features.GeneratorRuntime.Types.GeneratorHashes qualified as GeneratorHashes
import Logic.Features.Naming.Types.Name qualified as Name
import Logic.Features.ProjectModel.Types.ProjectModel qualified as ProjectFile
import Logic.Features.Reporting.Port (Warns (..))
import Logic.Features.Reporting.Types.Report (Report (..))
import Logic.Features.Staging.Port (Stages (..))
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import PGenieGen.Model.Output.Report qualified as Gen.Output.Report
import Utils.Prelude hiding (readFile, writeFile)

type Port m = (MonadParallel m, Stages m, Warns m, FsOps m, MonadError Report m, LoadsGen m)

data Artifact = Artifact
  { name :: Text,
    warnings :: [Gen.Output.Report],
    paths :: [Path]
  }

data Params = Params
  { projectFile :: ProjectFile.ProjectFile,
    project :: Gen.Input.Project
  }

data Result = Result
  { artifacts :: [Artifact]
  }

run :: (Port m) => Params -> m Result
run Params {projectFile, project} =
  stage "Generating" (length projectFile.artifacts) do
    existingHashes <- GeneratorHashes.tryLoadHashesFile

    artifactsWithHashes <-
      MonadParallel.forM projectFile.artifacts \artifact -> do
        let name = Name.inSnakeCase artifact.name
            genUrl = case artifact.gen of
              Gen.LocationUrl url -> url
              Gen.LocationPath path -> Path.toText path
            maybeHash = Map.lookup genUrl existingHashes
        stage name 2 do
          compileFnWithHash <-
            stage "Loading" 0 do
              (gen, newHash) <- loadGen artifact.gen maybeHash
              case gen artifact.config of
                Left errMsg ->
                  throwError
                    ( Report
                        []
                        errMsg
                        (Just "Ensure the artifact configuration conforms to the format expected by the generator")
                        [ ("config", to (Aeson.Text.encodeToTextBuilder artifact.config))
                        ]
                    )
                Right compileFn ->
                  pure (compileFn, genUrl, newHash)

          stage "Compiling" 0 do
            let (compileFn, genUrl, newHash) = compileFnWithHash
                output = compileFn project
            case output.result of
              Gen.Output.ErrResult report ->
                throwError
                  ( Report
                      report.path
                      report.message
                      Nothing
                      [ ( "warnings",
                          output.warnings
                            & map Gen.Output.Report.toWarningYamlText
                            & Text.intercalate "\n"
                        )
                      ]
                  )
              Gen.Output.OkResult generatedFiles -> do
                artifactPath <- case Path.maybeFromText name of
                  Nothing ->
                    throwError
                      ( Report
                          []
                          "Invalid artifact name"
                          (Just "Must be in snake_case and must not start with a number")
                          [("name", name)]
                      )
                  Just path ->
                    pure ("artifacts" <> path)
                generatedFilePaths <- for generatedFiles \file -> do
                  let modifiedPath = artifactPath <> file.path
                  writeFile modifiedPath file.content
                  pure modifiedPath
                pure (Artifact name output.warnings generatedFilePaths, (genUrl, newHash))

    let (artifacts, hashPairs) = unzip artifactsWithHashes
        updatedHashes = Map.union (Map.fromList hashPairs) existingHashes
        noNewHashes = null hashPairs

    unless noNewHashes do
      GeneratorHashes.writeHashesFile updatedHashes

    pure Result {artifacts}
