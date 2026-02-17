module Infra
  ( run,
  )
where

import Base.Prelude
import Data.Text.IO qualified as Text
import Fx
import Infra.Adapters.Main qualified as MainAdapter
import Logic qualified
import TextBuilder qualified

run :: (Logic.Event -> IO ()) -> Fx MainAdapter.Device Logic.Error () -> IO ()
run emitEvent fx =
  fx
    & scoping (MainAdapter.scope emitEvent)
    & handleErr
      ( \err -> do
          runTotalIO \() -> emitEvent (Logic.Failed err)
      )
    & Fx.runFx
