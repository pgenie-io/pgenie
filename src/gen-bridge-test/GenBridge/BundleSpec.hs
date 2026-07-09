module GenBridge.BundleSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text.IO qualified as Text
import GenBridge qualified as GenBridge
import GenBridge.Fixtures.Project1 qualified as Fixtures.Project1
import GenBridge.Model.Input qualified as Input
import GenBridge.Model.Output qualified as Output
import GenBridge.Model.Output.Report qualified as Output.Report
import System.Exit
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "bundle" do
    it "imports a Dhall generator at compile time and compiles the Project1 fixture into config-driven output files" do
      compile <-
        case gen (Just configJson) of
          Left err -> do
            putStrLn "Failed to parse config JSON:"
            Text.putStrLn err
            exitFailure
          Right compile -> pure compile

      let output =
            compile Fixtures.Project1.input

      files <-
        case output of
          Output.ErrOutput report -> do
            putStrLn "Generation failed!"
            Text.putStrLn (Output.Report.toErrorYamlText report)
            exitFailure
          Output.OkOutput (Output.OutputOk {warnings, value}) -> do
            putStrLn "Generation succeeded!"
            forM_ warnings \warning -> do
              Text.putStrLn (Output.Report.toWarningYamlText warning)

            pure value

      shouldBe
        files
        [ Output.File
            { path = "output.yaml",
              content =
                "config:\n\
                \  foo: Foo!\n\
                \  bar: null\n"
            }
        ]

gen :: Maybe Aeson.Value -> Either Text (Input.Project -> Output.Output)
gen =
  $$( GenBridge.bundle
        (GenBridge.LocationPath "./src/gen-bridge-test/Gen.dhall")
        Nothing
    )

configJson :: Aeson.Value
configJson =
  [aesonQQ|
    {
      "foo": "Foo!"
    }
  |]
