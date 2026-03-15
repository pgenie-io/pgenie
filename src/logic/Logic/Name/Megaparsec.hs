module Logic.Name.Megaparsec where

import Data.Text qualified as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Prelude hiding (many)

type Parser = Parsec Void Text

toTextParser :: Parser a -> (Text -> Either Text a)
toTextParser p = first (fromString . errorBundlePretty) . runParser (p <* eof) ""

complete :: Parser a -> Parser a
complete parser = parser <* eof

parts :: Parser [Text]
parts = do
  firstWord <-
    Text.cons
      <$> satisfy isAsciiLower
      <*> takeWhileP (Just "part tail") (\c -> isAsciiLower c || isDigit c)

  tail <- many do
    _ <- char '_'
    takeWhileP (Just "tail part") (\c -> isAsciiLower c || isDigit c)

  return (firstWord : tail)
