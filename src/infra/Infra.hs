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

run :: (Logic.Event -> IO ()) -> Fx MainAdapter.Device Logic.Error a -> (a -> IO ()) -> IO ()
run emitEvent fx handleResult = do
  result <-
    fx
      & scoping (MainAdapter.scope emitEvent)
      & exposeErr
      & Fx.runFx
  case result of
    Left err -> emitEvent (Logic.Failed err)
    Right a -> handleResult a
