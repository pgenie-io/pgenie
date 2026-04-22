module Infra
  ( run,
  )
where

import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Fx
import Infra.Adapters.Main qualified as MainAdapter
import Logic qualified
import System.Exit qualified as Exit
import Utils.Prelude

run :: (Logic.Event -> IO ()) -> Maybe Text -> Fx MainAdapter.Device Logic.Error Text -> IO ()
run emitEvent maybeDatabaseUrl fx = do
  result <-
    fx
      & scoping (MainAdapter.scope emitEvent maybeDatabaseUrl)
      & exposeErr
      & Fx.runFx
  case result of
    Left err -> do
      emitEvent (Logic.Failed err)
      Exit.exitFailure
    Right text -> unless (Text.null text) (TextIO.putStrLn text)
