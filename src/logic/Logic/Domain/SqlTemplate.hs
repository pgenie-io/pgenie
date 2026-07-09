module Logic.Domain.SqlTemplate
  ( SqlTemplate,
    toGenQueryFragments,
    toGenParamNames,
    render,
    tryFromText,
    spec,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GenBridge.Model.Input qualified as Gen
import Logic.Domain.Name qualified as Name
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck qualified as Qc
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Utils.Prelude

-- | Structured representation of a SQL template.
--
-- Allows for precise control over whitespace literals and parameters.
newtype SqlTemplate
  = SqlTemplate [Segment]
  deriving newtype (Eq, Show)

data Segment
  = Param Name.Name
  | Newline
  | -- | Non-empty whitespace that is not a newline. We want to preserve it for better formatting, but we want to distinguish it from newlines for rendering.
    LineWhitespace Text
  | -- | Text that does not contain whitespace.
    NonWhitespace Text
  | -- | We need to distinguish literals to avoid capturing params inside them.
    SingleQuotedLiteral Text
  | DoubleQuotedLiteral Text
  | -- | Text following @--@ up to (but excluding) the terminating newline.
    InlineComment Text
  | -- | Text between @/*@ and @*/@, excluding the delimiters.
    MultilineComment Text
  deriving stock (Eq, Show)

instance Qc.Arbitrary SqlTemplate where
  arbitrary =
    normalizeArbitrary
      <$> SqlTemplate
      <$> Qc.listOf arbitrary

  shrink (SqlTemplate segments) =
    normalizeArbitrary
      <$> SqlTemplate
      <$> Qc.shrink segments

instance Qc.Arbitrary Segment where
  arbitrary =
    Qc.oneof
      [ Param <$> arbitrary,
        pure Newline,
        LineWhitespace . onfrom <$> Qc.listOf1 (Qc.elements [' ', '\t']),
        -- Excludes `-` and `/` too: a lone `-` or `/` at the end of one
        -- `NonWhitespace` segment can combine with the start of an adjacent
        -- segment to form `--` or `/*`, which the parser always treats as
        -- the start of a comment. Since segments here are placed next to
        -- each other with no separator, allowing those characters would let
        -- `arbitrary` construct SqlTemplates that don't round-trip.
        NonWhitespace . onfrom <$> Qc.listOf1 (Qc.arbitrary `Qc.suchThat` (\c -> not (isSpace c) && c /= '$' && c /= '\'' && c /= '"' && c /= ':' && c /= '-' && c /= '/')),
        SingleQuotedLiteral . onfrom <$> Qc.listOf (Qc.arbitrary `Qc.suchThat` (/= '\'')),
        DoubleQuotedLiteral . onfrom <$> Qc.listOf (Qc.arbitrary `Qc.suchThat` (/= '"')),
        InlineComment . onfrom <$> Qc.listOf (Qc.arbitrary `Qc.suchThat` (\c -> c /= '\n' && c /= '\r')),
        MultilineComment . onfrom <$> Qc.listOf (Qc.arbitrary `Qc.suchThat` (\c -> c /= '*' && c /= '/'))
      ]

  shrink segment =
    case segment of
      Param name ->
        Param <$> Qc.shrink name
      Newline ->
        []
      LineWhitespace text ->
        [ LineWhitespace shrunkText
        | shrunkText <- Qc.shrink text,
          not (Text.null shrunkText)
        ]
      NonWhitespace text ->
        [ NonWhitespace shrunkText
        | shrunkText <- Qc.shrink text,
          not (Text.null shrunkText)
        ]
      SingleQuotedLiteral text ->
        SingleQuotedLiteral . onfrom <$> Qc.shrink text
      DoubleQuotedLiteral text ->
        DoubleQuotedLiteral . onfrom <$> Qc.shrink text
      InlineComment text ->
        InlineComment . onfrom <$> Qc.shrink text
      MultilineComment text ->
        MultilineComment . onfrom <$> Qc.shrink text

instance IsString SqlTemplate where
  fromString str =
    case Megaparsec.parse megaparsecOf "" (fromString str) of
      Left err -> error ("Failed to parse template: " <> show err)
      Right template -> template

-- |
-- Renders the template, replacing params with their rendered values. The second argument is a function that takes a param name and its first appearance index and renders it. The index is needed to support repeated params, which should be rendered the same way.
render ::
  -- | Keep newlines.
  Bool ->
  -- | Param renderer by name and first appearance index.
  (Name.Name -> Int -> TextBuilder) ->
  SqlTemplate ->
  TextBuilder
render keepWhitespace renderParam (SqlTemplate segments) =
  foldr step end segments Map.empty 0 ""
  where
    end = mempty
    step segment next indices count newlineHanger =
      case segment of
        NonWhitespace text ->
          newlineHanger <> from text <> next indices count ""
        Param name ->
          case Map.lookup name indices of
            Just index ->
              go index indices count
            Nothing ->
              go count (Map.insert name count indices) (succ count)
          where
            go index newIndices newCount =
              newlineHanger <> renderParam name index <> next newIndices newCount ""
        Newline ->
          if keepWhitespace
            then "\n" <> next indices count ""
            else next indices count " "
        LineWhitespace text ->
          if keepWhitespace
            then newlineHanger <> from text <> next indices count ""
            else " " <> next indices count ""
        SingleQuotedLiteral text ->
          newlineHanger <> "'" <> from text <> "'" <> next indices count ""
        DoubleQuotedLiteral text ->
          newlineHanger <> "\"" <> from text <> "\"" <> next indices count ""
        InlineComment text ->
          newlineHanger <> "--" <> from text <> next indices count ""
        MultilineComment text ->
          newlineHanger <> "/*" <> from text <> "*/" <> next indices count ""

toGenQueryFragments :: SqlTemplate -> [Gen.QueryFragment]
toGenQueryFragments (SqlTemplate segments) =
  normalizeFragments $ concat $ evalState (traverse segmentToFragment segments) (Map.empty, 0 :: Int)
  where
    segmentToFragment segment = do
      (indices, count) <- get
      case segment of
        NonWhitespace text -> do
          return [Gen.SqlQueryFragment text]
        Param name -> do
          case Map.lookup name indices of
            Just index -> do
              return
                [ Gen.VarQueryFragment
                    ( Gen.Var
                        { name = Name.toGenName name,
                          rawName = Name.toText name,
                          paramIndex = fromIntegral index
                        }
                    )
                ]
            Nothing -> do
              put (Map.insert name count indices, succ count)
              segmentToFragment (Param name)
        Newline -> do
          return [Gen.SqlQueryFragment "\n"]
        LineWhitespace text -> do
          return [Gen.SqlQueryFragment text]
        SingleQuotedLiteral text -> do
          return [Gen.SqlQueryFragment ("'" <> text <> "'")]
        DoubleQuotedLiteral text -> do
          return [Gen.SqlQueryFragment ("\"" <> text <> "\"")]
        InlineComment text -> do
          return [Gen.SqlQueryFragment ("--" <> text)]
        MultilineComment text -> do
          return [Gen.SqlQueryFragment ("/*" <> text <> "*/")]

    normalizeFragments :: [Gen.QueryFragment] -> [Gen.QueryFragment]
    normalizeFragments = \case
      Gen.SqlQueryFragment left : Gen.SqlQueryFragment right : rest ->
        normalizeFragments (Gen.SqlQueryFragment (left <> right) : rest)
      fragment : rest ->
        fragment : normalizeFragments rest
      [] -> []

toGenParamNames :: SqlTemplate -> [Name.Name]
toGenParamNames (SqlTemplate segments) =
  nub [name | Param name <- segments]

data ParamStyle
  = DollarParamStyle
  | ColonParamStyle
  deriving stock (Eq, Show)

megaparsecOf :: Megaparsec.Parsec Void Text SqlTemplate
megaparsecOf = do
  segmentsAndStyles <- Megaparsec.many segmentParser
  case nub (mapMaybe snd segmentsAndStyles) of
    _ : _ : _ ->
      fail "SQL template mixes `$name` and `:name` parameter styles. Use only one style per template."
    _ ->
      pure ()
  pure (normalizeParsed (SqlTemplate (fmap fst segmentsAndStyles)))
  where
    segmentParser :: Megaparsec.Parsec Void Text (Segment, Maybe ParamStyle)
    segmentParser =
      Megaparsec.choice
        [ (,Nothing) <$> lineCommentParser,
          (,Nothing) <$> blockCommentParser,
          (,Just DollarParamStyle) <$> dollarParamParser,
          (,Nothing) <$> castOperatorParser,
          (,Just ColonParamStyle) <$> colonParamParser,
          (,Nothing) <$> singleQuotedLiteralParser,
          (,Nothing) <$> doubleQuotedLiteralParser,
          (,Nothing) <$> newlineParser,
          (,Nothing) <$> lineWhitespaceParser,
          (,Nothing) <$> literalColonParser,
          (,Nothing) <$> nonWhitespaceParser
        ]

    lineCommentParser =
      Megaparsec.label "line comment" do
        Megaparsec.try do
          Megaparsec.string "--"
        content <- Megaparsec.takeWhileP (Just "line comment") (\c -> c /= '\n' && c /= '\r')
        pure (InlineComment content)

    blockCommentParser =
      Megaparsec.label "block comment" do
        Megaparsec.try do
          Megaparsec.string "/*"
        content <- Megaparsec.manyTill Megaparsec.anySingle (Megaparsec.string "*/")
        pure (MultilineComment (onfrom content))

    dollarParamParser =
      Megaparsec.label "dollar-parameter" do
        Megaparsec.try do
          Megaparsec.char '$'
        name <- Name.megaparsecOf
        pure (Param name)

    -- `::` must never be treated as the start of a colon-parameter (in either
    -- direction), so any two adjacent colons are consumed together as a
    -- literal unit before a lone `:` ever gets a chance to match. This also
    -- protects the second colon of a `::` pair from being mistaken for the
    -- start of a fresh param, since it's never reached on its own.
    castOperatorParser =
      Megaparsec.label "cast-operator" do
        Megaparsec.try do
          _ <- Megaparsec.char ':'
          Megaparsec.char ':'
        pure (NonWhitespace "::")

    colonParamParser =
      Megaparsec.label "colon-parameter" do
        name <- Megaparsec.try do
          _ <- Megaparsec.char ':'
          Name.megaparsecOf
        pure (Param name)

    -- A `:` that isn't part of a `::` cast and isn't followed by a valid
    -- identifier (e.g. array-slice syntax like `arr[1:2]`) falls back to
    -- literal text rather than failing the parse.
    literalColonParser = do
      Megaparsec.char ':'
      pure (NonWhitespace ":")

    newlineParser =
      Megaparsec.label "newline" do
        asum
          [ do
              _ <- Megaparsec.try do
                Megaparsec.char '\r'
              asum
                [ Megaparsec.try do
                    Megaparsec.char '\n'
                    pure (),
                  pure ()
                ],
            Megaparsec.try do
              Megaparsec.char '\n'
              pure ()
          ]
        pure Newline

    lineWhitespaceParser = do
      ws <- Megaparsec.takeWhile1P (Just "whitespace") (\c -> c == ' ' || c == '\t')
      pure (LineWhitespace ws)

    singleQuotedLiteralParser = do
      Megaparsec.try do
        Megaparsec.char '\''
      content <- Megaparsec.takeWhileP (Just "single-quoted literal") (/= '\'')
      Megaparsec.char '\''
      pure (SingleQuotedLiteral content)

    doubleQuotedLiteralParser = do
      Megaparsec.try do
        Megaparsec.char '"'
      content <- Megaparsec.takeWhileP (Just "double-quoted literal") (/= '"')
      Megaparsec.char '"'
      pure (DoubleQuotedLiteral content)

    -- Consumed one character at a time (rather than via `takeWhile1P`) so that
    -- a `--` or `/*` appearing mid-token (e.g. `1--comment`, `x/*comment*/`)
    -- stops the run and hands control back to the comment parsers, instead of
    -- being swallowed as literal text.
    nonWhitespaceParser = do
      chars <- Megaparsec.some do
        Megaparsec.notFollowedBy (Megaparsec.choice [Megaparsec.string "--", Megaparsec.string "/*"])
        Megaparsec.satisfy (\c -> not (isSpace c) && c /= '$' && c /= '\'' && c /= '"' && c /= ':')
      pure (NonWhitespace (onfrom chars))

normalizeParsed :: SqlTemplate -> SqlTemplate
normalizeParsed =
  SqlTemplate . dropLeadingWhitespace . dropTrailingWhitespace . coerce
  where
    dropLeadingWhitespace [] = []
    dropLeadingWhitespace (segment : rest) =
      case segment of
        LineWhitespace _ -> dropLeadingWhitespace rest
        Newline -> dropLeadingWhitespace rest
        _ -> segment : rest

    dropTrailingWhitespace = reverse . dropLeadingWhitespace . reverse

normalizeArbitrary :: SqlTemplate -> SqlTemplate
normalizeArbitrary (SqlTemplate segments) =
  SqlTemplate (terminateInlineComments (dropLeadingWhitespace (foldr step [] segments)))
  where
    -- An `InlineComment` extends to the next newline (or end of input) when
    -- rendered and reparsed, so any segment placed right after one with no
    -- `Newline` in between would be silently absorbed into the comment on
    -- reparse. Insert one where it's missing so `arbitrary` never produces a
    -- SqlTemplate that can't round-trip.
    terminateInlineComments = \case
      [] -> []
      segment@(InlineComment _) : rest ->
        segment : case rest of
          [] -> []
          Newline : _ -> terminateInlineComments rest
          _ -> Newline : terminateInlineComments rest
      segment : rest ->
        segment : terminateInlineComments rest

    step segment acc =
      case segment of
        LineWhitespace left ->
          case acc of
            [] -> [] -- Drop trailing whitespace
            LineWhitespace right : rest ->
              LineWhitespace (left <> right) : rest
            _ ->
              segment : acc
        Newline ->
          case acc of
            [] -> [] -- Drop trailing newlines
            _ -> segment : acc
        NonWhitespace left ->
          case acc of
            NonWhitespace right : rest ->
              NonWhitespace (left <> right) : rest
            _ ->
              segment : acc
        Param name ->
          case acc of
            NonWhitespace text : rest ->
              Param name : LineWhitespace " " : NonWhitespace text : rest
            _ ->
              segment : acc
        _ ->
          segment : acc

    dropLeadingWhitespace [] = []
    dropLeadingWhitespace (segment : rest) =
      case segment of
        LineWhitespace _ -> dropLeadingWhitespace rest
        Newline -> dropLeadingWhitespace rest
        _ -> segment : rest

tryFromText :: Text -> Either Text SqlTemplate
tryFromText text =
  case Megaparsec.parse (megaparsecOf <* Megaparsec.eof) "" text of
    Left err -> Left (Text.pack (Megaparsec.errorBundlePretty err))
    Right template -> Right template

spec :: Spec
spec = do
  describe "render" do
    it "renders a simple template without params" do
      let template = "SELECT 1"
      let rendered = render True (\_ _ -> "?") template
      rendered `shouldBe` "SELECT 1"

    it "renders a template with a single param" do
      let template = "SELECT $user_id"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "SELECT $1"

    it "renders a template with multiple params" do
      let template = "SELECT * FROM users WHERE id = $user_id AND name = $user_name"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "SELECT * FROM users WHERE id = $1 AND name = $2"

    it "handles repeated params with same index" do
      let template = "SELECT $user_id, $user_id"
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
      let template = "SELECT '$user_id'"
      let rendered = render True (\_ _ -> "PARAM") template
      rendered `shouldBe` "SELECT '$user_id'"

    it "does not interpret params inside line comments" do
      let template = "-- fall back when $cursor_id is absent\nSELECT $cursor_id"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "-- fall back when $cursor_id is absent\nSELECT $1"

    it "does not interpret params inside block comments" do
      let template = "/* $cursor_id is optional */ SELECT $cursor_id"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "/* $cursor_id is optional */ SELECT $1"

    it "does not interpret params inside an inline line comment with no preceding whitespace" do
      let template = "SELECT 1-- fall back when $cursor_id is absent"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "SELECT 1-- fall back when $cursor_id is absent"

    it "does not interpret params inside an inline block comment with no preceding whitespace" do
      let template = "SELECT 1/* $cursor_id is optional */FROM x"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "SELECT 1/* $cursor_id is optional */FROM x"

    it "ends a param name at a line comment starting right after it" do
      let template = "SELECT $cursor_id--fallback"
      let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "SELECT $1--fallback"

    it "does not create phantom params from commented placeholders" do
      let template = "-- fall back when $cursor_id is absent\n-- also mentions $notaparam\nSELECT id FROM action_runs WHERE id > $cursor_id"
      let paramNames = toGenParamNames template
      paramNames `shouldBe` ["cursor_id"]

    it "does not interpret colon-style params inside comments" do
      let template = "-- use :cursor_id\nSELECT id FROM action_runs WHERE id > :cursor_id"
      let rendered = render True (\_ i -> ":" <> to @TextBuilder (Text.pack (show (i + 1)))) template
      rendered `shouldBe` "-- use :cursor_id\nSELECT id FROM action_runs WHERE id > :1"

  describe "megaparsecOf" do
    it "parses a simple SQL query" do
      let input = "SELECT 1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse simple query"
        Right _ -> pure ()

    it "parses a query with a parameter" do
      let input = "SELECT $user_id"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse query with parameter"
        Right _ -> pure ()

    it "parses a query with multiple parameters" do
      let input = "SELECT * FROM users WHERE id = $user_id AND name = $user_name"
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
      let input = "SELECT '$user_id'"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse param inside quotes"
        Right template -> do
          let rendered = render True (\_ _ -> "REPLACED") template
          rendered `shouldBe` "SELECT '$user_id'"

    it "parses empty quoted strings" do
      let input = "SELECT ''"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse empty quoted string"
        Right _ -> pure ()

    it "parses line comments" do
      let input = "-- a comment\nSELECT 1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse line comment"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "-- a comment\nSELECT 1"

    it "parses block comments" do
      let input = "/* a comment */ SELECT 1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse block comment"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "/* a comment */ SELECT 1"

    it "parses block comments spanning multiple lines" do
      let input = "/* line 1\nline 2 */ SELECT 1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse multiline block comment"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "/* line 1\nline 2 */ SELECT 1"

    it "parses an inline line comment with no preceding whitespace" do
      let input = "SELECT 1-- a comment"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse inline line comment"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT 1-- a comment"

    it "parses an inline block comment with no preceding whitespace" do
      let input = "SELECT 1/* a comment */FROM x"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse inline block comment"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT 1/* a comment */FROM x"

    it "does not absorb a trailing CR into a line comment" do
      let input = "-- a comment\r\nSELECT 1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse line comment with CRLF"
        Right template -> do
          let rendered = render False (\_ _ -> "?") template
          rendered `shouldBe` "-- a comment SELECT 1"

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

    it "parses a query with a colon-style parameter" do
      let input = "SELECT :user_id"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left err -> expectationFailure ("Failed to parse query with colon parameter: " <> Megaparsec.errorBundlePretty err)
        Right template -> do
          let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
          rendered `shouldBe` "SELECT $1"

    it "handles repeated colon-style params with same index" do
      let input = "SELECT :user_id, :user_id"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left err -> expectationFailure ("Failed to parse repeated colon parameter: " <> Megaparsec.errorBundlePretty err)
        Right template -> do
          let rendered = render True (\_ i -> "$" <> to @TextBuilder (Text.pack (show (i + 1)))) template
          rendered `shouldBe` "SELECT $1, $1"

    it "does not treat a `::` cast operator as a parameter" do
      let input = "SELECT x::text"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left err -> expectationFailure ("Failed to parse cast operator: " <> Megaparsec.errorBundlePretty err)
        Right template -> do
          let rendered = render True (\_ _ -> "PARAM") template
          rendered `shouldBe` "SELECT x::text"

    it "does not treat chained `::` casts as parameters" do
      let input = "SELECT x::int::text"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left err -> expectationFailure ("Failed to parse chained cast operators: " <> Megaparsec.errorBundlePretty err)
        Right template -> do
          let rendered = render True (\_ _ -> "PARAM") template
          rendered `shouldBe` "SELECT x::int::text"

    it "treats a colon not followed by a valid identifier as literal text" do
      let input = "SELECT arr[1:2]"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left err -> expectationFailure ("Failed to parse array-slice syntax: " <> Megaparsec.errorBundlePretty err)
        Right template -> do
          let rendered = render True (\_ _ -> "PARAM") template
          rendered `shouldBe` "SELECT arr[1:2]"

    it "rejects a template mixing dollar-style and colon-style parameters" do
      let input = "SELECT * FROM users WHERE id = $user_id AND name = :user_name"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected parsing to fail for mixed parameter styles"

  describe "normalize" do
    it "removes leading whitespace" do
      let input = "  SELECT 1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT 1"

    it "removes trailing whitespace" do
      let input = "SELECT 1  "
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT 1"

    it "removes leading newlines" do
      let input = "\n\nSELECT 1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT 1"

    it "removes trailing newlines" do
      let input = "SELECT 1\n\n"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT 1"

    it "removes both leading and trailing whitespace" do
      let input = "\n  SELECT 1  \n"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT 1"

    it "preserves internal whitespace" do
      let input = "  SELECT\n  1\n  FROM\n  users  "
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT\n  1\n  FROM\n  users"

    it "removes leading tabs and spaces" do
      let input = "\t  \tSELECT 1"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT 1"

    it "removes trailing tabs and spaces" do
      let input = "SELECT 1\t  \t"
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` "SELECT 1"

    it "handles template with only whitespace" do
      let input = "  \n\t  \n  "
      let result = Megaparsec.parse megaparsecOf "" input
      case result of
        Left _ -> expectationFailure "Failed to parse"
        Right template -> do
          let rendered = render True (\_ _ -> "?") template
          rendered `shouldBe` ""

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
              Qc.counterexample
                ("Failed to parse rendered template:\n" <> Megaparsec.errorBundlePretty err)
                False
            Right parsedTemplate ->
              Qc.counterexample
                ("Rendered template:\n" <> to rendered)
                (sqlTemplate Qc.=== parsedTemplate)
    prop "parse and render idempotently with colon-style params" \(sqlTemplate :: SqlTemplate) ->
      let rendered =
            to
              ( render
                  True
                  (\name _ -> ":" <> to name)
                  sqlTemplate
              )
          parsed =
            Megaparsec.parse megaparsecOf "" rendered
       in case parsed of
            Left err ->
              Qc.counterexample
                ("Failed to parse rendered template:\n" <> Megaparsec.errorBundlePretty err)
                False
            Right parsedTemplate ->
              Qc.counterexample
                ("Rendered template:\n" <> to rendered)
                (sqlTemplate Qc.=== parsedTemplate)
