module Ui
  ( main,
  )
where

import Base.Prelude
import Logic qualified
import Ui.Commands qualified as Commands
import Ui.Framework qualified as Framework

-- |
-- Construct an application by specifying the runtime.
main ::
  (Logic.Caps m) =>
  -- | Execute an effect.
  (m () -> IO ()) ->
  -- | Application.
  IO ()
main =
  Framework.main
    "pgn"
    "pGenie CLI"
    [ Commands.generate
    ]
