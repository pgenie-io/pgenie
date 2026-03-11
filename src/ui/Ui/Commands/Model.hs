-- | Command that outputs the JSON project model transmitted to code generators.
--
-- Useful for debugging what gets produced during the generate command,
-- and for using the application as a source of analysis data.
module Ui.Commands.Model (model) where

import Base.Prelude
import Logic qualified
import Options.Applicative qualified as Opt
import Ui.Framework

model :: (Logic.Caps m) => Command m
model =
  Command
    { name = "model",
      description = "Output the project model transmitted to code generators",
      parser,
      execute
    }

data Params = Params
  { format :: Logic.ModelFormat
  }

parser :: Opt.Parser Params
parser =
  Params
    <$> Opt.option
      readFormat
      ( Opt.long "format"
          <> Opt.metavar "FORMAT"
          <> Opt.help "Output format: json or dhall"
      )
  where
    readFormat =
      Opt.eitherReader \s -> case s of
        "json" -> Right Logic.ModelFormatJson
        "dhall" -> Right Logic.ModelFormatDhall
        _ -> Left ("Unknown format: " <> s <> ". Expected 'json' or 'dhall'.")

execute :: (Logic.Caps m) => Params -> m Text
execute params = Logic.model params.format
