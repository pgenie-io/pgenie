module NameSpec (spec) where

import Logic.Features.Name
import Test.Hspec
import Utils.Prelude

spec :: Spec
spec = do
  describe "tryFromText" do
    it "parses lowercase snake_case names" do
      let result = tryFromText "album_format"
      result `shouldSatisfy` isRight

    it "parses uppercase single words" do
      let result = tryFromText "Vinyl"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "vinyl"

    it "parses uppercase with hyphens" do
      let result = tryFromText "DVD-Audio"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "dvd_audio"

    it "parses all-caps names" do
      let result = tryFromText "SACD"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "sacd"

    it "parses CD correctly" do
      let result = tryFromText "CD"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "cd"

    it "parses PascalCase names" do
      let result = tryFromText "Digital"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "digital"

    it "preserves lowercase names" do
      let result = tryFromText "cassette"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "cassette"

    it "handles mixed case correctly" do
      let result = tryFromText "Cassette"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "cassette"

    it "handles kebab-case by converting to snake_case" do
      let result = tryFromText "album-format"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "album_format"

    it "parses numeric tail parts" do
      let result = tryFromText "album_2024"
      result `shouldSatisfy` isRight
      fmap toText result `shouldBe` Right "album_2024"

    it "treats mixed alphanumeric parts as multiple parts" do
      tryFromText "box2d" `shouldBe` Right "box_2_d"
      tryFromText "float8" `shouldBe` Right "float_8"
