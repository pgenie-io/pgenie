module Logic.Name.Megaparsec where

import Base.Prelude hiding (many)
import Data.Text qualified as Text
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

toTextParser :: Parser a -> (Text -> Either Text a)
toTextParser p = first (fromString . errorBundlePretty) . runParser (p <* eof) ""

complete :: Parser a -> Parser a
complete parser = parser <* eof

parts :: Parser [Text]
parts =
  sepBy1 part (char '-')
  where
    part =
      Text.cons
        <$> satisfy isAsciiLower
        <*> takeWhileP (Just "part tail") (\c -> isAsciiLower c || isDigit c)
