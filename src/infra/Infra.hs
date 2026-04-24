module Infra
  ( run,
  )
where

import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Fx
import Infra.Adapters.Main qualified as MainAdapter
import Interpreters.Emitting qualified as Emitting
import Logic qualified
import System.Exit qualified as Exit
import Utils.Prelude

run :: (Emitting.Observation -> IO ()) -> Maybe Text -> Emitting.Emitting (Fx MainAdapter.Device Logic.Report) Text -> IO ()
run emitObservation maybeDatabaseUrl fx = do
  result <-
    Emitting.interpretEmitting fx
      & scoping (MainAdapter.scope emitObservation maybeDatabaseUrl)
      & exposeErr
      & Fx.runFx
  case result of
    Left err -> do
      emitObservation (Emitting.ExecutionFailed err)
      Exit.exitFailure
    Right text -> unless (Text.null text) (TextIO.putStrLn text)
