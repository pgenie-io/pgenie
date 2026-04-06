module ProjectFileSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Logic.Error qualified as Error
import Logic.ProjectFile
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
          result = tryFromYaml yaml :: Either Error.Error ProjectFile
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
    it "leaves image unset when not specified" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0\n\
            \artifacts:\n\
            \  java:\n\
            \    gen: https://raw.githubusercontent.com/pgenie-io/java.gen/v0.1.2/gen/Gen.dhall"
          result = tryFromYaml yaml :: Either Error.Error ProjectFile
      case result of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right pf -> pf.postgres `shouldBe` Nothing
    it "parses an explicit image setting" do
      let yaml =
            "space: my_space\n\
            \name: music_catalogue\n\
            \version: 1.0.0\n\
            \postgres: 15\n\
            \artifacts:\n\
            \  java:\n\
            \    gen: https://raw.githubusercontent.com/pgenie-io/java.gen/v0.1.2/gen/Gen.dhall"
          result = tryFromYaml yaml :: Either Error.Error ProjectFile
      case result of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right pf -> pf.postgres `shouldBe` Just 15
