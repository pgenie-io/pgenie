module SqlTemplateSpec (spec) where

import AppLogic.SqlTemplate
import Base.Prelude
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Megaparsec qualified as Megaparsec

spec :: Spec
spec = do
  describe "render" do
    it "renders a simple template without params" do
      let template = "SELECT 1"
      let rendered = render True (\_ _ -> "?") template
      rendered `shouldBe` "SELECT 1"

    it "renders a template with a single param" do
      let template = "SELECT $user-id"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "SELECT $1"

    it "renders a template with multiple params" do
      let template = "SELECT * FROM users WHERE id = $user-id AND name = $user-name"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "SELECT * FROM users WHERE id = $1 AND name = $2"

    it "handles repeated params with same index" do
      let template = "SELECT $user-id, $user-id"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "SELECT $1, $1"

    it "preserves newlines when keepWhitespace is True" do
      let template = "SELECT\n1"
      let rendered = render True (\_ _ -> "?") template
      rendered `shouldBe` "SELECT\n1"

    it "replaces newlines with spaces when keepWhitespace is False" do
      let template = "SELECT\n1"
      let rendered = render False (\_ _ -> "?") template
      rendered `shouldBe` "SELECT 1"

    it "preserves line whitespace when keepWhitespace is True" do
      let template = "SELECT  1"
      let rendered = render True (\_ _ -> "?") template
      rendered `shouldBe` "SELECT  1"

    it "collapses line whitespace to single space when keepWhitespace is False" do
      let template = "SELECT  1"
      let rendered = render False (\_ _ -> "?") template
      rendered `shouldBe` "SELECT 1"

    it "renders single-quoted literals with quotes" do
      let template = "SELECT 'hello world'"
      let rendered = render True (\_ _ -> "?") template
      rendered `shouldBe` "SELECT 'hello world'"

    it "renders double-quoted literals with quotes" do
      let template = "SELECT \"column_name\""
      let rendered = render True (\_ _ -> "?") template
      rendered `shouldBe` "SELECT \"column_name\""

    it "does not interpret params inside single-quoted literals" do
      let template = "SELECT '$user-id'"
      let rendered = render True (\_ _ -> "PARAM") template
      rendered `shouldBe` "SELECT '$user-id'"

  describe "megaparsecOf" do
    it "parses a simple SQL query" do
      let input = "SELECT 1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse simple query"
        Right _ -> pure ()

    it "parses a query with a parameter" do
      let input = "SELECT $user-id"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse query with parameter"
        Right _ -> pure ()

    it "parses a query with multiple parameters" do
      let input = "SELECT * FROM users WHERE id = $user-id AND name = $user-name"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse query with multiple parameters"
        Right _ -> pure ()

    it "parses newlines" do
      let input = "SELECT\n1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse newlines"
        Right _ -> pure ()

    it "parses CRLF as newlines" do
      let input = "SELECT\r\n1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse CRLF"
        Right _ -> pure ()

    it "parses single-quoted literals" do
      let input = "SELECT 'hello world'"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse single-quoted literal"
        Right _ -> pure ()

    it "parses double-quoted literals" do
      let input = "SELECT \"column_name\""
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse double-quoted literal"
        Right _ -> pure ()

    it "parses params inside quotes as literal text" do
      let input = "SELECT '$user-id'"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse param inside quotes"
        Right template -> do
          -- Verify that the param is not interpreted by checking the render output
          let rendered = render True (\_ _ -> "REPLACED") template
          rendered `shouldBe` "SELECT '$user-id'"

    it "parses empty quoted strings" do
      let input = "SELECT ''"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse empty quoted string"
        Right _ -> pure ()

    it "handles tabs as whitespace" do
      let input = "SELECT\t1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse tabs"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT\t1"

    it "parses multiple consecutive whitespace characters" do
      let input = "SELECT  \t  1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse multiple whitespace"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT  \t  1"

  describe "roundtrip property" do
    prop "parse and render idempotently" \(sqlTemplate :: SqlTemplate) ->
      let rendered =
            to
              ( render
                  True
                  (\name _ -> "$" <> to name)
                  sqlTemplate
              )
          parsed =
            Megaparsec.parse megaparsecOf "" rendered
       in case parsed of
            Left err ->
              counterexample
                ("Failed to parse rendered template:\n" <> Megaparsec.errorBundlePretty err)
                False
            Right parsedTemplate ->
              counterexample
                ("Rendered template:\n" <> to rendered)
                (sqlTemplate === parsedTemplate)
