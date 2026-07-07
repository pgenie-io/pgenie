module Logic.Procedures.GenerateCode where

import AlgebraicPath qualified as Path
import Control.Monad.Parallel qualified as MonadParallel
import Data.Aeson.Text qualified as Aeson.Text
import Data.Map.Strict qualified as Map
import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.GeneratorRuntime (LoadsGen (..))
import Logic.Capabilities.Reporting (Warns (..))
import Logic.Capabilities.Staging (Stages (..))
import Logic.Domain.GeneratorHashes qualified as GeneratorHashes
import Logic.Domain.Name qualified as Name
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.Report (Report (..))
import PGenieGen qualified as Gen
import PGenieGen.Model.Input qualified as Gen.Input
import PGenieGen.Model.Output qualified as Gen.Output
import Test.Hspec
import Utils.Prelude hiding (readFile, writeFile)

type Port m = (MonadParallel m, Stages m, Warns m, FsOps m, MonadError Report m, LoadsGen m)

data Artifact = Artifact
  { name :: Text,
    warnings :: [Gen.Output.Report],
    paths :: [Path]
  }
  deriving stock (Eq, Show)

data Params = Params
  { projectFile :: ProjectFile.ProjectFile,
    project :: Gen.Input.Project
  }

data Result = Result
  { artifacts :: [Artifact]
  }
  deriving stock (Eq, Show)

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
            for_ output.warnings \genReport ->
              warn
                ( Report
                    { path = genReport.path,
                      message = genReport.message,
                      suggestion = Nothing,
                      details = []
                    }
                )
            case output.result of
              Gen.Output.ErrResult report ->
                throwError
                  ( Report
                      report.path
                      report.message
                      Nothing
                      []
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
  readFile _ = throwError (Report [] "readFile not implemented in tests" Nothing [])
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

spec :: Spec
spec = do
  describe "run" do
    it "emits generator warnings on success" do
      let warning = Gen.Output.Report {path = ["unit", "foo"], message = "Unsupported unit skipped"}
          genReport = Report {path = ["unit", "foo"], message = "Unsupported unit skipped", suggestion = Nothing, details = []}
          gen = stubGen warning (Gen.Output.OkResult [])
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

    it "emits generator warnings before failure and omits the warnings blob" do
      let warning = Gen.Output.Report {path = ["unit", "bar"], message = "Unsupported unit skipped"}
          genReport = Report {path = ["unit", "bar"], message = "Unsupported unit skipped", suggestion = Nothing, details = []}
          errorReport = Gen.Output.Report {path = ["err"], message = "boom"}
          gen = stubGen warning (Gen.Output.ErrResult errorReport)
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
      state.warnings `shouldBe` [genReport]

stubGen :: Gen.Output.Report -> Gen.Output.Result -> Gen.Gen
stubGen warning result _config = Right (\_project -> Gen.Output.Output {warnings = [warning], result = result})
