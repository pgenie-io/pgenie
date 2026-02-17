module Logic.SqlTemplate
  ( SqlTemplate,
    toGenQueryFragments,
    toGenParamNames,
    render,
    megaparsecOf,
    tryFromText,
  )
where

import Base.Prelude
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Logic.Name qualified as Name
import PGenieGen.Model.Input qualified as Gen
import Test.QuickCheck qualified as Qc
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec

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
        NonWhitespace . onfrom <$> Qc.listOf1 (Qc.arbitrary `Qc.suchThat` (\c -> not (isSpace c) && c /= '$' && c /= '\'' && c /= '"')),
        SingleQuotedLiteral . onfrom <$> Qc.listOf (Qc.arbitrary `Qc.suchThat` (/= '\'')),
        DoubleQuotedLiteral . onfrom <$> Qc.listOf (Qc.arbitrary `Qc.suchThat` (/= '"'))
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

toGenQueryFragments :: SqlTemplate -> [Gen.QueryFragment]
toGenQueryFragments (SqlTemplate segments) =
  concat $ evalState (traverse segmentToFragment segments) (Map.empty, 0 :: Int)
  where
    segmentToFragment segment = do
      (indices, count) <- get
      case segment of
        NonWhitespace text -> do
          return [Gen.QueryFragmentSql text]
        Param name -> do
          case Map.lookup name indices of
            Just index -> do
              return
                [ Gen.QueryFragmentVar
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
          return [Gen.QueryFragmentSql "\n"]
        LineWhitespace text -> do
          return [Gen.QueryFragmentSql text]
        SingleQuotedLiteral text -> do
          return [Gen.QueryFragmentSql ("'" <> text <> "'")]
        DoubleQuotedLiteral text -> do
          return [Gen.QueryFragmentSql ("\"" <> text <> "\"")]

toGenParamNames :: SqlTemplate -> [Name.Name]
toGenParamNames (SqlTemplate segments) =
  nub [name | Param name <- segments]

megaparsecOf :: Megaparsec.Parsec Void Text SqlTemplate
megaparsecOf =
  normalizeParsed . SqlTemplate <$> Megaparsec.many segmentParser
  where
    segmentParser =
      Megaparsec.choice
        [ paramParser,
          singleQuotedLiteralParser,
          doubleQuotedLiteralParser,
          newlineParser,
          lineWhitespaceParser,
          nonWhitespaceParser
        ]

    paramParser =
      Megaparsec.label "dollar-parameter" do
        Megaparsec.try do
          Megaparsec.char '$'
        name <- Name.megaparsecOf
        pure (Param name)

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

    nonWhitespaceParser = do
      text <- Megaparsec.takeWhile1P (Just "non-whitespace") (\c -> not (isSpace c) && c /= '$' && c /= '\'' && c /= '"')
      pure (NonWhitespace text)

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
  SqlTemplate (dropLeadingWhitespace (foldr step [] segments))
  where
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
