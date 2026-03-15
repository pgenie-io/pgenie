module Logic.SyntaxAnalyser
  ( resolveText,
    module Data,
  )
where

import Data.Text qualified as Text
import Logic.SyntaxAnalyser.AstInterpreter qualified as AstInterpreter
import Logic.SyntaxAnalyser.Data as Data
import PostgresqlSyntax.Parsing qualified as Parsing
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
        (True, _, (beforeEnd, afterEnd)) ->
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
