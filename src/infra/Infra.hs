module Infra
  ( run,
  )
where

import Base.Prelude
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Fx
import Infra.Adapters.Main qualified as MainAdapter
import Logic qualified

run :: (Logic.Event -> IO ()) -> Fx MainAdapter.Device Logic.Error Text -> IO ()
run emitEvent fx = do
  result <-
    fx
      & scoping (MainAdapter.scope emitEvent)
      & exposeErr
      & Fx.runFx
  case result of
    Left err -> emitEvent (Logic.Failed err)
    Right text -> unless (Text.null text) (TextIO.putStrLn text)
