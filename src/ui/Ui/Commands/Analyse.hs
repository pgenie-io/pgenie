-- | Command that validates the project and optionally outputs the model.
--
-- When run without flags it performs the same checks as the generate command
-- without writing any files. When @--format@ is provided the project model is
-- serialised to stdout in the chosen format, which is useful for debugging and
-- for using the application as a source of analysis data.
module Ui.Commands.Analyse (analyse) where

import Base.Prelude
import Logic qualified
import Options.Applicative qualified as Opt
import Ui.Framework

analyse :: (Logic.Caps m) => Command m
analyse =
  Command
    { name = "analyse",
      description = "Validate the project without generating code; optionally output the project model",
      parser,
      execute
    }

data Params = Params
  { failOnSeqScans :: Bool,
    format :: Maybe Logic.ModelFormat
  }

parser :: Opt.Parser Params
parser =
  Params
    <$> Opt.switch
      ( Opt.long "fail-on-seq-scans"
          <> Opt.help "Fail the procedure if sequential scans are detected (instead of emitting warnings)"
      )
    <*> optional
      ( Opt.option
          readFormat
          ( Opt.long "format"
              <> Opt.metavar "FORMAT"
              <> Opt.help "Also output the project model in the given format: json or dhall"
          )
      )
  where
    readFormat =
      Opt.eitherReader \s -> case s of
        "json" -> Right Logic.ModelFormatJson
        "dhall" -> Right Logic.ModelFormatDhall
        _ -> Left ("Unknown format: " <> s <> ". Expected 'json' or 'dhall'.")

execute :: (Logic.Caps m) => Params -> m Text
execute params =
  Logic.analyse
    Logic.AnalyseOptions {failOnSeqScans = params.failOnSeqScans}
    params.format
