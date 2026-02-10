module SqlTemplate where

import Base.Name qualified as Name
import Base.Prelude
import Data.Map.Strict qualified as Map
import Test.QuickCheck qualified as Qc

newtype SqlTemplate
  = SqlTemplate [Segment]

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

instance Qc.Arbitrary SqlTemplate where
  arbitrary =
    SqlTemplate <$> Qc.listOf arbitrary

  shrink (SqlTemplate segments) =
    SqlTemplate <$> Qc.shrink segments

instance Qc.Arbitrary Segment where
  arbitrary =
    Qc.oneof
      [ Param <$> arbitrary,
        pure Newline,
        LineWhitespace . onfrom <$> Qc.listOf1 (Qc.elements [' ', '\t']),
        NonWhitespace . onfrom <$> Qc.listOf1 (Qc.arbitrary `Qc.suchThat` (\c -> not (isSpace c) && c /= '$')),
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
        LineWhitespace . onfrom <$> Qc.shrink text
      NonWhitespace text ->
        NonWhitespace . onfrom <$> Qc.shrink text
      SingleQuotedLiteral text ->
        SingleQuotedLiteral . onfrom <$> Qc.shrink text
      DoubleQuotedLiteral text ->
        DoubleQuotedLiteral . onfrom <$> Qc.shrink text

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
