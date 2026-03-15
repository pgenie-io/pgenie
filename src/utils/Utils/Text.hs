module Utils.Text where

import Data.Text qualified as Text
import Utils.Prelude

pointToLocation :: Text -> Int -> Text
pointToLocation queryText errorPos =
  let queryLines = Text.lines queryText
      (lineIdx, columnIdx) = findErrorLocation queryLines errorPos
      linesWithPointer = insertPointerAfterLine queryLines lineIdx columnIdx
   in Text.unlines linesWithPointer
  where
    findErrorLocation :: [Text] -> Int -> (Int, Int)
    findErrorLocation lines errorPos = go lines 1 0
      where
        go [] currentPos _ = (length lines, 1)
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
