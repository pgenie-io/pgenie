module Logic.Features.Name.Megaparsec
  ( Parser,
    toTextParser,
    parts,
    complete,
  )
where

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
  tailParts <- many do
    _ <- optional separator
    tailPart

  pure (firstWord : tailParts)
  where
    separator =
      char '_' <|> char '-'

    wordPart =
      Text.toLower <$> takeWhile1P (Just "word part") isAsciiLetter

    tailPart =
      asum
        [ takeWhile1P (Just "number tail part") isDigit,
          wordPart
        ]

    isAsciiLetter character =
      isAsciiLower character || isAsciiUpper character
