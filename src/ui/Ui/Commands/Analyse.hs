-- | Command that validates the project and optionally outputs the model.
--
-- When run without flags it performs the same checks as the generate command
-- without writing any files. When @--format@ is provided the project model is
-- serialised to stdout in the chosen format, which is useful for debugging and
-- for using the application as a source of analysis data.
module Ui.Commands.Analyse (analyse) where

import Logic.Workflows.Analyse qualified as Analyse
import Options.Applicative qualified as Opt
import Ui.Framework
import Utils.Prelude

analyse :: (Analyse.Port m) => Command m
analyse =
  Command
    { name = "analyse",
      description = "Validate the project without generating code; optionally output the project model",
      parser,
      execute = Analyse.run
    }

type Params = Analyse.Params

parser :: Opt.Parser Params
parser =
  Analyse.Params
    <$> Opt.switch
      ( Opt.long "fail-on-seq-scans"
          <> Opt.help "Fail the procedure if sequential scans are detected (instead of emitting warnings)"
      )
    <*> optional
      ( Opt.option
          readFormat
          ( Opt.long "output"
              <> Opt.metavar "OUTPUT"
              <> Opt.help "Also output the project model in the given format: json or dhall"
          )
      )
  where
    readFormat =
      Opt.eitherReader \s -> case s of
        "json" -> Right Analyse.ModelFormatJson
        "dhall" -> Right Analyse.ModelFormatDhall
        _ -> Left ("Unknown format: " <> s <> ". Expected 'json' or 'dhall'.")
