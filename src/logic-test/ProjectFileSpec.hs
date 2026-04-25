module ProjectFileSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Logic.Features.ProjectFile
import Logic.Features.Report qualified as Report
import Test.Hspec
import Utils.Prelude

spec :: Spec
spec = do
  describe "tryFromYaml" do
    it "parses boolean config values as Aeson.Bool, not Aeson.String" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0\n\
            \artifacts:\n\
            \  java:\n\
            \    gen: https://raw.githubusercontent.com/pgenie-io/java.gen/v0.1.2/gen/Gen.dhall\n\
            \    config:\n\
            \      useOptional: true"
          result = tryFromYaml yaml :: Either Report.Report ProjectFile
      case result of
        Left err ->
          expectationFailure ("Parse failed: " <> show err)
        Right pf -> do
          art <- case pf.artifacts of
            [art] -> pure art
            _ -> fail "Expected exactly one artifact"
          art.config
            `shouldBe` Just
              ( Aeson.Object
                  (KeyMap.fromList [(Key.fromText "useOptional", Aeson.Bool True)])
              )

    it "parses a missing artifacts section as an empty list" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0"
          result = tryFromYaml yaml :: Either Report.Report ProjectFile
      case result of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right pf -> do
          length pf.artifacts `shouldBe` 0
          pf.postgres `shouldBe` Nothing

    it "leaves image unset when not specified" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0\n\
            \"
          result = tryFromYaml yaml :: Either Report.Report ProjectFile
      case result of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right pf -> do
          length pf.artifacts `shouldBe` 0
          pf.postgres `shouldBe` Nothing

    it "parses an explicit image setting" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0\n\
            \postgres: 15\n\
            \"
          result = tryFromYaml yaml :: Either Report.Report ProjectFile
      case result of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right pf -> do
          length pf.artifacts `shouldBe` 0
          pf.postgres `shouldBe` Just 15
