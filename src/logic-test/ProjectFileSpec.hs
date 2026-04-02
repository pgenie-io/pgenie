module ProjectFileSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
import Logic.Algebra qualified as Algebra
import Logic.ProjectFile
import Test.Hspec
import Utils.Prelude

spec :: Spec
spec = do
  describe "tryFromYaml" do
    it "parses boolean config values as Aeson.Bool, not Aeson.String" do
      let yaml =
            Text.unlines
              [ "space: my_space",
                "name: music_catalogue",
                "version: 1.0.0",
                "artifacts:",
                "  java:",
                "    gen: https://raw.githubusercontent.com/pgenie-io/java.gen/v0.1.2/gen/Gen.dhall",
                "    config:",
                "      useOptional: true"
              ]
          result = tryFromYaml yaml :: Either Algebra.Error ProjectFile
      case result of
        Left err ->
          expectationFailure ("Parse failed: " <> show err)
        Right pf -> do
          length pf.artifacts `shouldBe` 1
          let art = head pf.artifacts
          art.config
            `shouldBe` Just
              ( Aeson.Object
                  (KeyMap.fromList [(Key.fromText "useOptional", Aeson.Bool True)])
              )
