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
  firstWord <- wordPart
  tail <- many do
    _ <- char '_'
    tailPart

  return (firstWord : tail)
  where
    wordPart =
      Text.cons
        <$> satisfy isAsciiLower
        <*> takeWhileP (Just "word tail") isAsciiLower

    tailPart =
      asum
        [ takeWhile1P (Just "number tail part") isDigit,
          wordPart
        ]
