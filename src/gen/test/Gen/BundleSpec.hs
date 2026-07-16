{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Gen.BundleSpec (spec) where

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
  describe "bundle" do
    it "imports a Dhall generator at compile time and compiles the Project1 fixture into config-driven output files" do
      compile <-
        case gen (Just configJson) of
          Left err -> do
            putStrLn "Failed to parse config JSON:"
            Text.putStrLn err
            Exit.exitFailure
          Right compile -> pure compile

      let output =
            compile Fixtures.input1

      files <-
        case output of
          Gen.ErrOutput report -> do
            putStrLn "Generation failed!"
            Text.putStrLn (Report.toYamlText "Error" report)
            Exit.exitFailure
          Gen.OkOutput (Gen.OutputOk {warnings, value}) -> do
            putStrLn "Generation succeeded!"
            forM_ warnings \warning -> do
              Text.putStrLn (Report.toYamlText "Warning" warning)

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

gen :: Maybe Aeson.Value -> Either Text (Gen.Project -> Gen.Output)
gen =
  $$( Gen.bundle
        (Gen.LocationPath "./test/Gen.dhall")
        Nothing
    )

configJson :: Aeson.Value
configJson =
  [aesonQQ|
    {
      "foo": "Foo!"
    }
  |]
