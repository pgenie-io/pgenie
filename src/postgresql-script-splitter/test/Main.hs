module Main (main) where

import Data.Text (Text)
import PostgresqlScriptSplitter
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "splitScript" do
  it "splits a single statement" do
    sqlOf <$> splitScript "select 1;" `shouldBe` Right ["select 1;"]

  it "splits two simple statements" do
    sqlOf <$> splitScript "select 1; select 2;" `shouldBe` Right ["select 1;", "select 2;"]

  it "drops a trailing empty statement after the last semicolon" do
    sqlOf <$> splitScript "select 1;\n" `shouldBe` Right ["select 1;"]

  it "handles a script with no trailing semicolon" do
    sqlOf <$> splitScript "select 1" `shouldBe` Right ["select 1"]

  it "drops an all-whitespace script" do
    sqlOf <$> splitScript "  \n\t  " `shouldBe` Right []

  it "trims surrounding whitespace off each statement" do
    sqlOf <$> splitScript "  select 1;  \n\n  select 2;  " `shouldBe` Right ["select 1;", "select 2;"]

  it "does not split on a semicolon inside a single-quoted string" do
    sqlOf <$> splitScript "select 'a;b';" `shouldBe` Right ["select 'a;b';"]

  it "handles a doubled single quote as an escaped literal quote" do
    sqlOf <$> splitScript "select 'it''s; fine';" `shouldBe` Right ["select 'it''s; fine';"]

  it "does not split on a semicolon inside a double-quoted identifier" do
    sqlOf <$> splitScript "select 1 as \"a;b\";" `shouldBe` Right ["select 1 as \"a;b\";"]

  it "handles a doubled double quote as an escaped literal quote" do
    sqlOf <$> splitScript "select 1 as \"a\"\"b;c\";" `shouldBe` Right ["select 1 as \"a\"\"b;c\";"]

  it "does not split on a semicolon inside an E'...' escape string" do
    sqlOf <$> splitScript "select E'a;b';" `shouldBe` Right ["select E'a;b';"]

  it "honors a backslash escape inside an E'...' string" do
    sqlOf <$> splitScript "select E'a\\'; select 2'; select 3;"
      `shouldBe` Right ["select E'a\\'; select 2';", "select 3;"]

  it "does not treat a backslash as an escape in a plain single-quoted string" do
    sqlOf <$> splitScript "select 'a\\'; select 2;"
      `shouldBe` Right ["select 'a\\';", "select 2;"]

  it "does not split on a semicolon inside a $$...$$ dollar-quoted body" do
    sqlOf <$> splitScript "do $$ begin perform 1; end; $$;" `shouldBe` Right ["do $$ begin perform 1; end; $$;"]

  it "does not split on a semicolon inside a tagged $tag$...$tag$ dollar-quoted body" do
    sqlOf <$> splitScript "do $body$ perform 1; $body$;" `shouldBe` Right ["do $body$ perform 1; $body$;"]

  it "does not treat a different tag's dollar-quote delimiter as closing" do
    sqlOf <$> splitScript "select $a$ literal $b$ text $a$;" `shouldBe` Right ["select $a$ literal $b$ text $a$;"]

  it "does not split on a semicolon inside a line comment" do
    sqlOf <$> splitScript "select 1; -- comment; with semicolon\nselect 2;"
      `shouldBe` Right ["select 1;", "-- comment; with semicolon\nselect 2;"]

  it "does not split on a semicolon inside a block comment" do
    sqlOf <$> splitScript "select 1; /* comment; with semicolon */ select 2;"
      `shouldBe` Right ["select 1;", "/* comment; with semicolon */ select 2;"]

  it "handles nested block comments" do
    sqlOf <$> splitScript "/* outer /* inner; */ still comment */ select 1;"
      `shouldBe` Right ["/* outer /* inner; */ still comment */ select 1;"]

  it "reproduces #74: two CREATE INDEX CONCURRENTLY statements split into separate statements" do
    sqlOf
      <$> splitScript
        "create index concurrently \"a_idx\" on a (x);\ncreate index concurrently \"b_idx\" on b (y);"
      `shouldBe` Right
        [ "create index concurrently \"a_idx\" on a (x);",
          "create index concurrently \"b_idx\" on b (y);"
        ]

  it "fails on an unterminated single-quoted string" do
    case splitScript "select 'a;" of
      Left err -> err.location.column `shouldBe` 8
      Right _ -> expectationFailure "Expected a SplitError"

  it "fails on an unterminated dollar-quoted body" do
    case splitScript "select $$ a" of
      Left err -> err.location.column `shouldBe` 8
      Right _ -> expectationFailure "Expected a SplitError"

  it "fails on an unterminated block comment" do
    case splitScript "select 1; /* unterminated" of
      Left err -> err.location.column `shouldBe` 11
      Right _ -> expectationFailure "Expected a SplitError"

  it "reports the location as line/column of the failing statement's start, for a later statement" do
    case splitScript "select 1;\nselect 'a;" of
      Left err -> (err.location.line, err.location.column) `shouldBe` (2, 8)
      Right _ -> expectationFailure "Expected a SplitError"

  it "reports each statement's source location" do
    case splitScript "select 1;\nselect 2;" of
      Right [s1, s2] -> do
        (s1.location.line, s1.location.column) `shouldBe` (1, 1)
        (s2.location.line, s2.location.column) `shouldBe` (2, 1)
      other -> expectationFailure ("Unexpected result: " <> show other)

sqlOf :: [Statement] -> [Text]
sqlOf = map (.sql)
