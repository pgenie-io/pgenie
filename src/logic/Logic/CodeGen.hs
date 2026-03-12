module Logic.CodeGen
  ( generateCode,
    GeneratedArtifact (..),
  )
where

import AlgebraicPath qualified as Path
import Base.Prelude hiding (readFile, writeFile)
import Control.Monad.Parallel qualified as MonadParallel
import Data.Aeson.Text qualified as Aeson.Text
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Logic.Algebra
import Logic.Dsl (Script, Stages (..))
import Logic.GeneratorHashes qualified as GeneratorHashes
import Logic.Name qualified as Name
import Logic.ProjectFile qualified as ProjectFile
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import PGenieGen.Model.Output.Report qualified as Gen.Output.Report

data GeneratedArtifact = GeneratedArtifact
  { name :: Text,
    warnings :: [Gen.Output.Report],
    filePaths :: [Path]
  }

generateCode :: ProjectFile.ProjectFile -> Gen.Input.Project -> Script [GeneratedArtifact]
generateCode projectFile project =
  stage "Generating" (length projectFile.artifacts) do
    -- Load existing hashes file
    existingHashes <- GeneratorHashes.tryLoadHashesFile

    -- Load generators and collect new hashes
    artifactsWithHashes <-
      MonadParallel.forM projectFile.artifacts \artifact -> do
        let name = Name.inSnakeCase artifact.name
            genUrl = locationToUrl artifact.gen
            maybeHash = Map.lookup genUrl existingHashes
        stage name 2 do
          compileFnWithHash <-
            stage "Loading" 0 do
              (gen, newHash) <- loadGen artifact.gen maybeHash
              case gen artifact.config of
                Left errMsg ->
                  throwError
                    ( Error
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
                  ( Error
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
                      ( Error
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
                pure ((GeneratedArtifact name output.warnings generatedFilePaths), (genUrl, newHash))

    -- Extract artifacts and hashes
    let (artifacts, hashPairs) = unzip artifactsWithHashes
        updatedHashes = Map.union (Map.fromList hashPairs) existingHashes
        noNewHashes = null hashPairs

    -- Write updated hashes file
    unless noNewHashes do
      writeFile "freeze1.pgn.yaml" (GeneratorHashes.serializeHashesMap updatedHashes)

    pure artifacts

locationToUrl :: Gen.Location -> Text
locationToUrl = \case
  Gen.LocationUrl url -> url
  Gen.LocationPath path -> Path.toText path
