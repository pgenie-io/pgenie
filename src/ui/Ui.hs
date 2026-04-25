module Ui
  ( main,
  )
where

import Interpreters.Observing qualified as Observing
import Logic qualified
import Logic.ProjectFile qualified as ProjectFile
import Ui.Commands qualified as Commands
import Ui.Display qualified as Display
import Ui.Framework qualified as Framework
import Utils.Prelude

-- |
-- Construct an application by specifying the runtime.
main ::
  (Logic.Caps m) =>
  -- | Version string for @--version@ (SemVer, without the PVP @0.@ prefix).
  Text ->
  -- | Execute an effect with an observation sink and an optional database URL.
  -- The effect receives the parsed project file.
  ((Observing.Observation -> IO ()) -> Maybe Text -> (ProjectFile.ProjectFile -> m Text) -> IO ()) ->
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
    (\dbUrl -> runEffect (Display.handleObservation display) dbUrl)
