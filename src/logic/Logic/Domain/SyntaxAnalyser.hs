-- |
-- Resolves a SQL query's syntax tree into a 'QuerySyntaxAnalysis' by parsing
-- it with @postgresql-syntax@ and interpreting the resulting AST.
module Logic.Domain.SyntaxAnalyser
  ( resolveText,
    module Data,
    spec,
  )
where

import Data.Text qualified as Text
import Logic.Domain.SyntaxAnalyser.AstInterpreter qualified as AstInterpreter
import Logic.Domain.SyntaxAnalyser.Data as Data
import PostgresqlSyntax.Parsing qualified as Parsing
import Test.Hspec
import Utils.Prelude

resolveText :: Text -> Either Text QuerySyntaxAnalysis
resolveText sql =
  case Parsing.run (Parsing.inSpace Parsing.preparableStmt) (stripComments sql) of
    Left reason -> Left (onto reason)
    Right ast -> AstInterpreter.preparableStmtQuerySyntaxAnalysis ast

-- | Strip SQL comments from the query.
-- Handles both single-line (--) and block (/* */) comments.
stripComments :: Text -> Text
stripComments = Text.unlines . map stripLineComment . removeBlockComments . Text.lines
  where
    -- Remove block comments (/* ... */)
    removeBlockComments :: [Text] -> [Text]
    removeBlockComments = go False []
      where
        go _ acc [] = reverse acc
        go inComment acc (line : rest) =
          let (inComment', line') = processLine inComment line
           in go inComment' (if Text.null line' then acc else line' : acc) rest

    processLine :: Bool -> Text -> (Bool, Text)
    processLine inComment line =
      case (inComment, Text.breakOn "/*" line, Text.breakOn "*/" line) of
        -- We're in a comment, look for end
        (True, _, (_beforeEnd, afterEnd)) ->
          if Text.null afterEnd
            then (True, "")
            else processLine False (Text.drop 2 afterEnd)
        -- Not in comment, look for start
        (False, (beforeStart, afterStart), _) ->
          if Text.null afterStart
            then (False, beforeStart)
            else
              let (inComment', rest') = processLine True (Text.drop 2 afterStart)
               in (inComment', beforeStart <> rest')

    -- Remove single-line comments (--)
    stripLineComment :: Text -> Text
    stripLineComment line = go "" line
      where
        go prefix remaining =
          case Text.breakOn "--" remaining of
            (before, after) ->
              if Text.null after
                then prefix <> remaining
                else
                  let fullBefore = prefix <> before
                   in if isInsideString fullBefore
                        then go (fullBefore <> Text.take 2 after) (Text.drop 2 after)
                        else Text.stripEnd fullBefore

    -- Simple heuristic: check if we're inside a string by counting unescaped single quotes
    isInsideString :: Text -> Bool
    isInsideString text =
      let quotes = Text.count "'" text - 2 * Text.count "''" text
       in odd quotes

spec :: Spec
spec = do
  describe "resolveText" do
    describe "handles SQL comments" do
      it "parses SELECT with single-line comment at the beginning" do
        let sql = "-- This is a comment\nSELECT 1"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

      it "parses SELECT with single-line comment in the middle" do
        let sql = "SELECT 1 -- inline comment"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

      it "parses SELECT with multiple single-line comments" do
        let sql = "-- Comment 1\n-- Comment 2\nSELECT 1\n-- Comment 3"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

      it "parses SELECT with block comment" do
        let sql = "/* This is a block comment */\nSELECT 1"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

      it "parses SELECT with multi-line block comment" do
        let sql = "/*\n * Multi-line\n * block comment\n */\nSELECT 1"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

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

    it "strips comment after string containing escaped quotes" do
      let sql = "SELECT 'it''s a test' -- this is a comment"
      case resolveText sql of
        Left err -> expectationFailure $ "Failed to parse: " <> to err
        Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

    it "strips comment after string containing double dashes" do
      let sql = "SELECT 'abc--def' -- real comment"
      case resolveText sql of
        Left err -> expectationFailure $ "Failed to parse: " <> to err
        Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

    describe "detects cardinality" do
      it "detects a single row from SELECT without FROM" do
        let sql = "SELECT $1, $2"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

      it "detects a single row from DISTINCT SELECT without FROM" do
        let sql = "SELECT DISTINCT $1, $2"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

      it "detects an optional row from SELECT without FROM and WHERE" do
        let sql = "SELECT $1 WHERE $2"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` UpToRowAmount 1

      it "detects single row from LIMIT 1" do
        let sql = "SELECT * FROM users LIMIT 1"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` UpToRowAmount 1

      it "preserves single-row cardinality when LIMIT exceeds it" do
        let sql = "SELECT 1 LIMIT 10"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 1

      it "detects multiple rows from LIMIT N" do
        let sql = "SELECT * FROM users LIMIT 10"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` UpToRowAmount 10

      it "detects zero rows from OFFSET beyond a single-row select" do
        let sql = "SELECT 1 OFFSET 1"
        case resolveText sql of
          Left err -> expectationFailure $ "Failed to parse: " <> to err
          Right analysis -> analysis.resultRowAmount `shouldBe` SpecificRowAmount 0

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

    it "detects multiple rows from INSERT SELECT RETURNING" do
      let sql = "INSERT INTO users (name) SELECT name FROM other_users RETURNING id"
      case resolveText sql of
        Left err -> expectationFailure $ "Failed to parse: " <> to err
        Right analysis -> analysis.resultRowAmount `shouldBe` AnyRowAmount
