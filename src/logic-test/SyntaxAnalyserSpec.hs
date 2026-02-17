module SyntaxAnalyserSpec (spec) where

import Base.Prelude
import Logic.SyntaxAnalyser
import Test.Hspec

spec :: Spec
spec = do
  describe "resolveText" do
    describe "detects cardinality" do
      it "detects single row from LIMIT 1" do
        let sql = "SELECT * FROM users LIMIT 1"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` UpToRowAmount 1

      it "detects multiple rows from LIMIT N" do
        let sql = "SELECT * FROM users LIMIT 10"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` UpToRowAmount 10

      it "detects any rows without LIMIT" do
        let sql = "SELECT * FROM users"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` AnyRowAmount

      it "detects zero rows from INSERT without RETURNING" do
        let sql = "INSERT INTO users (name) VALUES ('test')"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 0

      it "detects rows from INSERT with RETURNING" do
        let sql = "INSERT INTO users (name) VALUES ('test') RETURNING id"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1
