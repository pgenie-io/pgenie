-- |
-- Entry point wiring: assembles the CLI commands and the terminal display,
-- then hands both to the command-line framework.
module Ui
  ( main,
  )
where

import Interpreters.Observing qualified as Observing
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Procedures.AnalyseProject qualified as AnalyseProject
import Logic.Procedures.GenerateCode qualified as GenerateCode
import Ui.Commands qualified as Commands
import Ui.Display qualified as Display
import Ui.Framework qualified as Framework
import Utils.Prelude

-- |
-- Construct an application by specifying the runtime.
main ::
  (AnalyseProject.Port m, GenerateCode.Port m) =>
  -- | Version string for @--version@ (SemVer, without the PVP @0.@ prefix).
  Text ->
  -- | Execute an effect with an observation sink, an optional database URL,
  -- and whether to reuse a Docker container across runs.
  -- The effect receives the parsed project file.
  ((Observing.Observation -> IO ()) -> Maybe Text -> Bool -> (ProjectFile.ProjectFile -> m Text) -> IO ()) ->
  -- | Application.
  IO ()
main version runEffect = do
  display <- Display.new
  Framework.main
    "pgn"
    "pGenie CLI"
    ""
    version
    [ Commands.analyse,
      Commands.generate,
      Commands.manageIndexes
    ]
    (\dbUrl reuseContainer -> runEffect (Display.handleObservation display) dbUrl reuseContainer)
