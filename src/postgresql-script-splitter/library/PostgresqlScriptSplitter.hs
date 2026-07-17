-- |
-- Splits a PostgreSQL script (a sequence of semicolon-terminated SQL
-- statements, as found in a @psql -f@ input file) into its individual
-- statements, so that each can be sent to the server as its own message
-- instead of a single multi-statement Simple Query.
--
-- Statement boundaries are found by scanning for top-level semicolons,
-- correctly skipping over semicolons that appear inside single- and
-- double-quoted literals, @E'...'@ escape strings, dollar-quoted bodies,
-- line comments, and (nested) block comments.
module PostgresqlScriptSplitter
  ( Statement (..),
    SourceLocation (..),
    SplitError (..),
    splitScript,
  )
where

import Control.Applicative (many)
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

-- | A statement's position in the original script.
data SourceLocation = SourceLocation
  { -- | 1-based line number.
    line :: Int,
    -- | 1-based column number.
    column :: Int,
    -- | 0-based character offset.
    offset :: Int
  }
  deriving stock (Eq, Show)

-- | A single statement extracted from a script, with leading and trailing
-- whitespace trimmed.
data Statement = Statement
  { sql :: Text,
    location :: SourceLocation
  }
  deriving stock (Eq, Show)

-- | A script that could not be split, because it contains an unterminated
-- quoted string, dollar-quoted body, or block comment.
data SplitError = SplitError
  { location :: SourceLocation,
    message :: Text
  }
  deriving stock (Eq, Show)

-- | Split a script into its individual statements. Statements consisting
-- only of whitespace (e.g. a trailing empty statement after the script's
-- final semicolon) are dropped.
splitScript :: Text -> Either SplitError [Statement]
splitScript input =
  case M.parse (semicolonOffsetsParser <* M.eof) "" input of
    Left bundle -> Left (adaptError input bundle)
    Right semicolonOffsets -> Right (statementsFromOffsets input semicolonOffsets)

-- ---------------------------------------------------------------------------
-- Splitting the input using the found top-level semicolon offsets
-- ---------------------------------------------------------------------------

statementsFromOffsets :: Text -> [Int] -> [Statement]
statementsFromOffsets input = go 0
  where
    go start [] =
      statementAt input start (Text.drop start input)
    go start (semicolonOffset : rest) =
      let chunk = Text.take (semicolonOffset - start + 1) (Text.drop start input)
       in statementAt input start chunk <> go (semicolonOffset + 1) rest

statementAt :: Text -> Int -> Text -> [Statement]
statementAt input chunkStart chunk =
  let leadingWs = Text.length (Text.takeWhile isSpaceChar chunk)
      trimmed = Text.dropWhileEnd isSpaceChar (Text.drop leadingWs chunk)
   in if Text.null trimmed
        then []
        else [Statement {sql = trimmed, location = locate input (chunkStart + leadingWs)}]

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v'

-- | Compute the 1-based line/column for a 0-based character offset.
locate :: Text -> Int -> SourceLocation
locate input targetOffset =
  let before = Text.take targetOffset input
      linesBefore = Text.splitOn "\n" before
      lineNumber = length linesBefore
      columnNumber = Text.length (last linesBefore) + 1
   in SourceLocation {line = lineNumber, column = columnNumber, offset = targetOffset}

adaptError :: Text -> M.ParseErrorBundle Text Void -> SplitError
adaptError input bundle =
  let firstError :| _ = M.bundleErrors bundle
      msg = case firstError of
        M.FancyError _ fancySet ->
          case [msg' | M.ErrorFail msg' <- toList fancySet] of
            (msg' : _) -> Text.pack msg'
            [] -> Text.pack (M.parseErrorTextPretty firstError)
        _ -> Text.pack (M.parseErrorTextPretty firstError)
   in SplitError {location = locate input (M.errorOffset firstError), message = msg}

-- ---------------------------------------------------------------------------
-- Scanning for top-level semicolons
-- ---------------------------------------------------------------------------

type Scanner = Parsec Void Text

-- | Scans the whole input, returning the offset of every top-level
-- semicolon (one that isn't nested inside a quoted string, dollar-quoted
-- body, or comment).
semicolonOffsetsParser :: Scanner [Int]
semicolonOffsetsParser = do
  offsets <- many do
    M.try do
      skipAtoms
      offset <- M.getOffset
      _ <- MC.char ';'
      pure offset
  skipAtoms
  pure offsets
  where
    skipAtoms = M.skipMany atomParser

-- | Consumes one unit of "not a top-level semicolon": a comment, a quoted
-- construct, or a single character of plain SQL text.
atomParser :: Scanner ()
atomParser =
  M.choice
    [ lineCommentParser,
      blockCommentParser,
      escapeStringParser,
      singleQuotedParser,
      doubleQuotedParser,
      dollarQuotedParser,
      plainCharParser
    ]

lineCommentParser :: Scanner ()
lineCommentParser = do
  _ <- M.try (MC.string "--")
  _ <- M.takeWhileP (Just "line comment") (\c -> c /= '\n' && c /= '\r')
  pure ()

-- | Block comments nest in PostgreSQL, so @\/* a \/* b *\/ c *\/@ is a
-- single comment closed by the final @*\/@.
blockCommentParser :: Scanner ()
blockCommentParser = do
  start <- M.getOffset
  _ <- M.try (MC.string "/*")
  requireClosed start "Unterminated block comment" (closeBlockComment (1 :: Int))
  where
    closeBlockComment depth
      | depth <= 0 = pure ()
      | otherwise = do
          _ <- M.takeWhileP Nothing (\c -> c /= '*' && c /= '/')
          next <- M.anySingle
          case next of
            '*' -> do
              closed <- M.option False (True <$ M.try (MC.char '/'))
              if closed then closeBlockComment (depth - 1) else closeBlockComment depth
            '/' -> do
              opened <- M.option False (True <$ M.try (MC.char '*'))
              closeBlockComment (if opened then depth + 1 else depth)
            _ -> closeBlockComment depth

-- | An @E'...'@ or @e'...'@ escape string, where a backslash escapes the
-- next character (so @\\'@ does not close the string) in addition to the
-- standard doubled-quote escape.
escapeStringParser :: Scanner ()
escapeStringParser = do
  start <- M.getOffset
  _ <- M.try (MC.char' 'E' *> MC.char '\'')
  requireClosed start "Unterminated escape string" (quotedBody True '\'')

singleQuotedParser :: Scanner ()
singleQuotedParser = do
  start <- M.getOffset
  _ <- M.try (MC.char '\'')
  requireClosed start "Unterminated single-quoted string" (quotedBody False '\'')

doubleQuotedParser :: Scanner ()
doubleQuotedParser = do
  start <- M.getOffset
  _ <- M.try (MC.char '"')
  requireClosed start "Unterminated double-quoted identifier" (quotedBody False '"')

-- | Consumes the body of a quoted construct up to and including its
-- closing (undoubled) quote character. A doubled quote (@''@ or @""@) is
-- an escaped literal quote and does not close the construct; when
-- @allowBackslashEscape@ is set, a backslash also escapes the following
-- character.
quotedBody :: Bool -> Char -> Scanner ()
quotedBody allowBackslashEscape quoteChar = loop
  where
    loop = do
      _ <-
        M.takeWhileP
          Nothing
          (\c -> c /= quoteChar && not (allowBackslashEscape && c == '\\'))
      next <- M.anySingle
      if allowBackslashEscape && next == '\\'
        then M.anySingle *> loop
        else
          if next == quoteChar
            then do
              doubled <- M.option False (True <$ M.try (MC.char quoteChar))
              if doubled then loop else pure ()
            else loop -- unreachable: takeWhileP only stops at quoteChar or backslash

-- | A @$tag$...$tag$@ dollar-quoted body, where @tag@ may be empty (as in
-- the common @$$...$$@ form).
dollarQuotedParser :: Scanner ()
dollarQuotedParser = do
  start <- M.getOffset
  tag <- M.try do
    _ <- MC.char '$'
    tag <- M.takeWhileP (Just "dollar-quote tag") isTagChar
    _ <- MC.char '$'
    pure tag
  let closer = "$" <> tag <> "$"
  requireClosed
    start
    "Unterminated dollar-quoted string"
    (() <$ M.manyTill M.anySingle (M.try (MC.string closer)))
  where
    isTagChar c = isAlphaNum c || c == '_'

-- | A single character of plain SQL text: not the start of a comment,
-- quoted construct, or dollar-quote, and not a top-level semicolon.
plainCharParser :: Scanner ()
plainCharParser = do
  M.notFollowedBy (MC.string "--")
  M.notFollowedBy (MC.string "/*")
  M.notFollowedBy (MC.char' 'E' *> MC.char '\'')
  _ <- M.satisfy (\c -> c /= '\'' && c /= '"' && c /= '$' && c /= ';')
  pure ()

-- | Run @content@; if it fails to reach a closing delimiter (i.e. it runs
-- off the end of the input), report the failure at @start@ -- the position
-- of the opening delimiter -- rather than at the end of the input, which is
-- where the underlying parser failure would otherwise be attributed.
requireClosed :: Int -> String -> Scanner a -> Scanner a
requireClosed start label content = do
  result <- M.observing (M.try content)
  case result of
    Right a -> pure a
    Left _ -> do
      M.setOffset start
      fail label
