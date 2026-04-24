module Infra
  ( run,
  )
where

import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Fx
import Infra.Adapters.Main qualified as MainAdapter
import Infra.Adapters.Script qualified as Script
import Logic qualified
import System.Exit qualified as Exit
import Utils.Prelude

run :: (Script.Event -> IO ()) -> Maybe Text -> Script.ScriptT (Fx MainAdapter.Device Logic.Report) Text -> IO ()
run emitEvent maybeDatabaseUrl fx = do
  result <-
    Script.runScript fx
      & scoping (MainAdapter.scope emitEvent maybeDatabaseUrl)
      & exposeErr
      & Fx.runFx
  case result of
    Left err -> do
      emitEvent (Script.Failed err)
      Exit.exitFailure
    Right text -> unless (Text.null text) (TextIO.putStrLn text)
