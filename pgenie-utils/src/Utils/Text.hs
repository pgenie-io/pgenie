-- |
-- Text helpers shared across the project.
module Utils.Text
  ( pointToLocation,
  )
where

import Data.Text qualified as Text
import Utils.Prelude

-- | Renders 'queryText' with a @^@ pointer inserted on a line of its own
-- directly below the character at 'errorPos', for showing users where a
-- parse error occurred within a larger block of SQL.
pointToLocation :: Text -> Int -> Text
pointToLocation queryText errorPos =
  Text.unlines linesWithPointer
  where
    queryLines = Text.lines queryText
    (lineIdx, columnIdx) = findErrorLocation queryLines errorPos
    linesWithPointer = insertPointerAfterLine queryLines lineIdx columnIdx

    findErrorLocation :: [Text] -> Int -> (Int, Int)
    findErrorLocation lines errorPos = go lines 1 0
      where
        go [] _ _ = (length lines, 1)
        go (line : rest) currentPos lineIdx =
          let lineLength = Text.length line
              lineEnd = currentPos + lineLength
           in if errorPos <= lineEnd
                then (lineIdx, errorPos - currentPos + 1)
                else go rest (lineEnd + 1) (lineIdx + 1) -- +1 for newline
    insertPointerAfterLine :: [Text] -> Int -> Int -> [Text]
    insertPointerAfterLine lines lineIdx columnIdx =
      let (before, after) = splitAt lineIdx lines
          pointer = Text.replicate (max 0 (columnIdx - 1)) " " <> "^"
       in case after of
            [] -> before <> [pointer]
            (errorLine : rest) -> before <> [errorLine, pointer] <> rest
