-- | Command that validates the project and optionally outputs the model.
--
-- When run without flags it performs the same checks as the generate command
-- without writing any files. When @--format@ is provided the project model is
-- serialised to stdout in the chosen format, which is useful for debugging and
-- for using the application as a source of analysis data.
module Ui.Commands.Analyse (analyse) where

import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Procedures.Analyse qualified as Analyse
import Options.Applicative qualified as Opt
import Ui.Framework
import Utils.Prelude

analyse :: forall m. (Analyse.Port m) => Command m
analyse =
  Command
    { name = "analyse",
      description = "Validate the project without generating code; optionally output the project model",
      parser,
      execute
    }
  where
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

    execute :: ProjectFile.ProjectFile -> Params -> m Text
    execute projectFile params =
      Analyse.run
        Analyse.Params
          { projectFile,
            failOnSeqScans = params.failOnSeqScans,
            output = params.output
          }
        <&> (.outputText)

-- | Parsed command-line options for the @analyse@ command.
data Params = Params
  { failOnSeqScans :: Bool,
    output :: Maybe Analyse.ModelFormat
  }
