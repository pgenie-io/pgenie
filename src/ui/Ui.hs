module Ui
  ( main,
  )
where

import Base.Prelude
import Logic qualified
import Ui.Commands qualified as Commands
import Ui.Display qualified as Display
import Ui.Framework qualified as Framework

-- |
-- Construct an application by specifying the runtime.
main ::
  (Logic.Caps m) =>
  -- | Version string for @--version@ (SemVer, without the PVP @0.@ prefix).
  Text ->
  -- | Execute an effect with an event sink.
  ((Logic.Event -> IO ()) -> m Text -> IO ()) ->
  -- | Application.
  IO ()
main version runEffect = do
  display <- Display.new
  Framework.main
    "pgn"
    "pGenie CLI"
    "Recommended workflow: analyse -> generate -> (see seq-scan warnings) -> manage-indexes -> (commit migration) -> generate"
    version
    [ Commands.analyse,
      Commands.generate,
      Commands.manageIndexes
    ]
    (runEffect (Display.handleEvent display))
