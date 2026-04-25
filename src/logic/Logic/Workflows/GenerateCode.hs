module Logic.Workflows.GenerateCode where

import AlgebraicPath qualified as Path
import Control.Monad.Parallel qualified as MonadParallel
import Data.Aeson.Text qualified as Aeson.Text
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Logic.Features.Fs (FsOps (..))
import Logic.Features.GeneratorHashes qualified as GeneratorHashes
import Logic.Features.Name qualified as Name
import Logic.Features.ProjectFile qualified as ProjectFile
import Logic.Features.Report (Report (..), Warns (..))
import Logic.Features.Staging (Stages (..))
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import PGenieGen.Model.Output.Report qualified as Gen.Output.Report
import Utils.Prelude hiding (readFile, writeFile)

class (Monad m) => LoadsGen m where
  loadGen ::
    Gen.Location ->
    -- | Possible integrity hash for caching.
    Maybe Text ->
    -- | Action producing the gen along with its integrity hash.
    m (Gen.Gen, Text)

type Port m = (MonadParallel m, Stages m, Warns m, FsOps m, GeneratorHashes.Port m, LoadsGen m)

data Artifact = Artifact
  { name :: Text,
    warnings :: [Gen.Output.Report],
    paths :: [Path]
  }

run :: (Port m) => ProjectFile.ProjectFile -> Gen.Input.Project -> m [Artifact]
run projectFile project =
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
              Gen.Output.ResultErr report ->
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
              Gen.Output.ResultOk generatedFiles -> do
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

    pure artifacts
