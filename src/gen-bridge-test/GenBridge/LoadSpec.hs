{-# LANGUAGE QuasiQuotes #-}

module GenBridge.LoadSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text.IO qualified as Text
import GenBridge qualified as GenBridge
import GenBridge.Contract qualified as Gen
import GenBridge.Contract.Report qualified as Gen.Report
import GenContractV5.Fixtures.Project1 qualified as Fixtures.Project1
import System.Exit qualified as Exit
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "load" do
    it "loads a Dhall generator at runtime, computes its integrity hash, and compiles the Project1 fixture into config-driven output files" do
      (gen, hash) <-
        GenBridge.load location Nothing Text.putStrLn Text.putStrLn

      -- Print the computed hash for verification
      putStrLn $ "Computed hash: " ++ show hash

      compile <-
        case gen (Just configJson) of
          Left err -> do
            putStrLn "Failed to parse config JSON:"
            Text.putStrLn err
            Exit.exitFailure
          Right compile -> pure compile

      let output =
            compile Fixtures.Project1.input

      files <-
        case output of
          Gen.ErrOutput report -> do
            putStrLn "Generation failed!"
            Text.putStrLn (Gen.Report.toErrorYamlText report)
            Exit.exitFailure
          Gen.OkOutput (Gen.OutputOk {warnings, value}) -> do
            putStrLn "Generation succeeded!"
            forM_ warnings \warning -> do
              Text.putStrLn (Gen.Report.toWarningYamlText warning)

            pure value

      shouldBe
        files
        [ Gen.File
            { path = "output.yaml",
              content =
                "config:\n\
                \  foo: Foo!\n\
                \  bar: null\n"
            }
        ]

location :: GenBridge.Location
location =
  GenBridge.LocationPath "./src/gen-bridge-test/Gen.dhall"

configJson :: Aeson.Value
configJson =
  [aesonQQ|
    {
      "foo": "Foo!"
    }
  |]
