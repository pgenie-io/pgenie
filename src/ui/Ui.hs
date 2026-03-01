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
  -- | Execute an effect with an event sink.
  ((Logic.Event -> IO ()) -> m () -> IO ()) ->
  -- | Application.
  IO ()
main runEffect = do
  display <- Display.new
  Framework.main
    "pgn"
    "pGenie CLI"
    [ Commands.generate,
      Commands.model
    ]
    (runEffect (Display.handleEvent display))
