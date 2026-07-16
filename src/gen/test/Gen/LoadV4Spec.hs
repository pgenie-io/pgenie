{-# LANGUAGE QuasiQuotes #-}

module Gen.LoadV4Spec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text.IO qualified as Text
import Gen qualified
import GenContractV5.Fixtures qualified as Fixtures
import System.Exit qualified as Exit
import Test.Hspec
import Test.Report qualified as Report
import Prelude

spec :: Spec
spec = do
  describe "load" do
    it "loads a v4-contract-declared Dhall generator and compiles the (v5-shaped) Project1 fixture through the downgrade path" do
      (gen, _hash) <-
        Gen.load (Gen.LocationPath "./test/GenV4.dhall") Nothing Text.putStrLn Text.putStrLn

      compile <-
        case gen (Just configJson) of
          Left err -> do
            putStrLn "Failed to parse config JSON:"
            Text.putStrLn err
            Exit.exitFailure
          Right compile -> pure compile

      let output = compile Fixtures.input1

      files <-
        case output of
          Gen.ErrOutput report -> do
            putStrLn "Generation failed!"
            Text.putStrLn (Report.toYamlText "Error" report)
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
