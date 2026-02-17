module SyntaxAnalyserSpec (spec) where

import Base.Prelude
import Logic.SyntaxAnalyser
import Test.Hspec

spec :: Spec
spec = do
  describe "resolveText" do
    describe "handles SQL comments" do
      it "parses SELECT with single-line comment at the beginning" do
        let sql = "-- This is a comment\nSELECT 1"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` AnyRowAmount

      it "parses SELECT with single-line comment in the middle" do
        let sql = "SELECT 1 -- inline comment"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` AnyRowAmount

      it "parses SELECT with multiple single-line comments" do
        let sql = "-- Comment 1\n-- Comment 2\nSELECT 1\n-- Comment 3"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` AnyRowAmount

      it "parses SELECT with block comment" do
        let sql = "/* This is a block comment */\nSELECT 1"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` AnyRowAmount

      it "parses SELECT with multi-line block comment" do
        let sql = "/*\n * Multi-line\n * block comment\n */\nSELECT 1"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` AnyRowAmount

      it "parses complex query with comments from demo" do
        let sql =
              "-- Select albums with their full recording information\n\
              \select \n\
              \  album.id,\n\
              \  album.name,\n\
              \  album.released,\n\
              \  album.format,\n\
              \  album.recording\n\
              \from album\n\
              \where album.recording is not null\n\
              \order by album.released desc\n\
              \limit 10"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` UpToRowAmount 10

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
