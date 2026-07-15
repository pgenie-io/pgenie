{-# LANGUAGE QuasiQuotes #-}

module GenBridge.LoadV4Spec (spec) where

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
    it "loads a v4-contract-declared Dhall generator and compiles the (v5-shaped) Project1 fixture through the downgrade path" do
      (gen, _hash) <-
        GenBridge.load (GenBridge.LocationPath "./src/gen-bridge-test/GenV4.dhall") Nothing Text.putStrLn Text.putStrLn

      compile <-
        case gen (Just configJson) of
          Left err -> do
            putStrLn "Failed to parse config JSON:"
            Text.putStrLn err
            Exit.exitFailure
          Right compile -> pure compile

      let output = compile Fixtures.Project1.input

      files <-
        case output of
          Gen.ErrOutput report -> do
            putStrLn "Generation failed!"
            Text.putStrLn (Gen.Report.toErrorYamlText report)
            Exit.exitFailure
          Gen.OkOutput (Gen.OutputOk {value}) ->
            pure value

      shouldBe
        files
        [ Gen.File
            { path = "custom-type-names.yaml",
              content = "- status\n"
            }
        ]
  where
    configJson :: Aeson.Value
    configJson =
      [aesonQQ| { "foo": "Foo!" } |]
