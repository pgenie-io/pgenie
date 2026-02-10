module SqlTemplate where

import Base.Name qualified as Name
import Base.Prelude
import Data.Map.Strict qualified as Map

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

render ::
  -- | Keep newlines.
  Bool ->
  -- | Param renderer by name and first appearance index.
  (Name.Name -> Int -> TextBuilder) ->
  SqlTemplate ->
  TextBuilder
render keepNewlines renderParam (SqlTemplate segments) =
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
          if keepNewlines then "\n" <> next indices count "" else next indices count " "
        LineWhitespace text ->
          " " <> next indices count ""
        SingleQuotedLiteral text ->
          newlineHanger <> "'" <> from text <> "'" <> next indices count ""
        DoubleQuotedLiteral text ->
          newlineHanger <> "\"" <> from text <> "\"" <> next indices count ""
