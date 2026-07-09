-- |
-- Runs each configured artifact's generator against the resolved project
-- model, writing generated files to disk and tracking generator integrity
-- hashes to detect drift between runs.
module Logic.Procedures.GenerateCode
  ( Port,
    Params (..),
    Result (..),
    Artifact (..),
    run,
    spec,
  )
where

import AlgebraicPath qualified as Path
import Control.Monad.Parallel qualified as MonadParallel
import Data.Aeson.Text qualified as Aeson.Text
import Data.Map.Strict qualified as Map
import GenBridge qualified as Gen
import GenBridge.Model.Input qualified as Gen.Input
import GenBridge.Model.Output qualified as Gen.Output
import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.GeneratorRuntime (LoadsGen (..))
import Logic.Capabilities.Reporting (Warns (..))
import Logic.Capabilities.Staging (Stages (..))
import Logic.Domain.GeneratorHashes qualified as GeneratorHashes
import Logic.Domain.Name qualified as Name
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.Report (Report (..))
import Test.Hspec
import Utils.Prelude hiding (readFile, writeFile)

-- | Everything the code generation procedure needs from its execution context.
type Port m = (MonadParallel m, Stages m, Warns m, FsOps m, MonadError Report m, LoadsGen m)

-- | One project artifact's generation outcome: the files it wrote and any
-- warnings its generator emitted.
data Artifact = Artifact
  { name :: Text,
    warnings :: [Gen.Output.Report],
    paths :: [Path]
  }
  deriving stock (Eq, Show)

-- | Input to the code generation procedure.
data Params = Params
  { projectFile :: ProjectFile.ProjectFile,
    project :: Gen.Input.Project
  }

-- | Output of the code generation procedure.
data Result = Result
  { artifacts :: [Artifact]
  }
  deriving stock (Eq, Show)

-- | Generate code for every configured artifact in parallel, reusing
-- previously loaded generators when their integrity hash is unchanged.
run :: (Port m) => Params -> m Result
run Params {projectFile, project} =
  stage "Generating" (length projectFile.artifacts) do
    existingHashes <-
      catchError
        (GeneratorHashes.parseHashesFile <$> readFile GeneratorHashes.hashesFilePath)
        (\(_ :: Report) -> pure Map.empty)

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
            case output of
              Gen.Output.ErrOutput report ->
                throwError
                  ( Report
                      report.path
                      report.message
                      Nothing
                      []
                  )
              Gen.Output.OkOutput Gen.Output.OutputOk {warnings, value = generatedFiles} -> do
                for_ warnings \genReport ->
                  warn
                    ( Report
                        { path = genReport.path,
                          message = genReport.message,
                          suggestion = Nothing,
                          details = []
                        }
                    )
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
                  filePath <- case Path.maybeFromText file.path of
                    Nothing ->
                      throwError
                        ( Report
                            []
                            "Invalid generator output file path"
                            (Just "The generator produced a file path that is not a valid relative path")
                            [("path", file.path)]
                        )
                    Just path -> pure path
                  let modifiedPath = artifactPath <> filePath
                  writeFile modifiedPath file.content
                  pure modifiedPath
                pure (Artifact name warnings generatedFilePaths, (genUrl, newHash))

    let (artifacts, hashPairs) = unzip artifactsWithHashes
        updatedHashes = Map.union (Map.fromList hashPairs) existingHashes
        noNewHashes = null hashPairs

    unless noNewHashes do
      writeFile GeneratorHashes.hashesFilePath (GeneratorHashes.serializeHashesMap updatedHashes)

    pure Result {artifacts}

-- * Tests

newtype TestM a = TestM (ExceptT Report (ReaderT TestEnv IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader TestEnv, MonadError Report, MonadParallel)

data TestEnv = TestEnv
  { gen :: Gen.Gen,
    state :: IORef TestState
  }

data TestState = TestState
  { warnings :: [Report],
    writtenFiles :: [(Path, Text)]
  }

runTestM :: Gen.Gen -> TestM a -> IO (Either Report a, TestState)
runTestM gen (TestM m) = do
  ref <- newIORef (TestState [] [])
  result <- runReaderT (runExceptT m) (TestEnv gen ref)
  state <- readIORef ref
  pure (result, state)

instance Stages TestM where
  stage _ _ (TestM m) = TestM m

instance Warns TestM where
  warn report = TestM do
    env <- ask
    liftIO
      $ modifyIORef'
        env.state
        ( \s ->
            TestState
              { warnings = s.warnings ++ [report],
                writtenFiles = s.writtenFiles
              }
        )

instance FsOps TestM where
  readFile _ = throwError "readFile not implemented in tests"
  writeFile path content = TestM do
    env <- ask
    liftIO
      $ modifyIORef'
        env.state
        ( \s ->
            TestState
              { warnings = s.warnings,
                writtenFiles = s.writtenFiles ++ [(path, content)]
              }
        )
  listDir _ = pure []

instance LoadsGen TestM where
  loadGen _ _ = do
    env <- ask
    pure (env.gen, "hash")

-- | Test suite for the code generation procedure.
spec :: Spec
spec = do
  describe "run" do
    it "emits generator warnings on success" do
      let warning = Gen.Output.Report {path = ["unit", "foo"], message = "Unsupported unit skipped"}
          genReport = Report {path = ["unit", "foo"], message = "Unsupported unit skipped", suggestion = Nothing, details = []}
          gen = stubGen (Gen.Output.OkOutput Gen.Output.OutputOk {warnings = [warning], value = []})
          projectFile =
            ProjectFile.ProjectFile
              { space = "space",
                name = "project",
                version = Gen.Input.Version 0 0 0,
                artifacts = [ProjectFile.Artifact {name = "my_artifact", gen = Gen.LocationUrl "http://example.com/gen", config = Nothing}],
                postgres = Nothing
              }
          project =
            Gen.Input.Project
              { space = Name.toGenName "space",
                name = Name.toGenName "project",
                version = Gen.Input.Version 0 0 0,
                customTypes = [],
                queries = [],
                migrations = []
              }
          params = Params {projectFile, project}
      (result, state) <- runTestM gen (run params)
      result `shouldBe` Right Result {artifacts = [Artifact {name = "my_artifact", warnings = [warning], paths = []}]}
      state.warnings `shouldBe` [genReport]

    it "fails without emitting warnings when the generator errors" do
      let errorReport = Gen.Output.Report {path = ["err"], message = "boom"}
          gen = stubGen (Gen.Output.ErrOutput errorReport)
          projectFile =
            ProjectFile.ProjectFile
              { space = "space",
                name = "project",
                version = Gen.Input.Version 0 0 0,
                artifacts = [ProjectFile.Artifact {name = "my_artifact", gen = Gen.LocationUrl "http://example.com/gen", config = Nothing}],
                postgres = Nothing
              }
          project =
            Gen.Input.Project
              { space = Name.toGenName "space",
                name = Name.toGenName "project",
                version = Gen.Input.Version 0 0 0,
                customTypes = [],
                queries = [],
                migrations = []
              }
          params = Params {projectFile, project}
      (result, state) <- runTestM gen (run params)
      result `shouldBe` Left (Report {path = ["err"], message = "boom", suggestion = Nothing, details = []})
      state.warnings `shouldBe` []
  where
    stubGen :: Gen.Output.Output -> Gen.Gen
    stubGen output _config = Right (const output)
