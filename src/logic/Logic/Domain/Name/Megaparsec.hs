module Logic.Domain.Name.Megaparsec
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
  -- Wrapped in `try`: a trailing separator not followed by a valid part
  -- (e.g. the `-` opening a `--` comment right after a param name, as in
  -- `$foo--comment`) must not consume input, or `many` can't stop cleanly
  -- and the whole parse fails instead of ending the name before the `-`.
  tailParts <- many (Text.Megaparsec.try (optional separator *> tailPart))

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
