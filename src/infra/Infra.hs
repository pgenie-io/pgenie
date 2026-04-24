module Infra
  ( run,
  )
where

import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Fx
import Infra.Adapters.Main qualified as MainAdapter
import Logic qualified
import Runtime.Emitting qualified as Emitting
import Runtime.Observation qualified as Observation
import System.Exit qualified as Exit
import Utils.Prelude

run :: (Observation.Observation -> IO ()) -> Maybe Text -> Emitting.Emitting (Fx MainAdapter.Device Logic.Report) Text -> IO ()
run emitObservation maybeDatabaseUrl fx = do
  result <-
    Emitting.interpretEmitting fx
      & scoping (MainAdapter.scope emitObservation maybeDatabaseUrl)
      & exposeErr
      & Fx.runFx
  case result of
    Left err -> do
      emitObservation (Observation.ExecutionFailed err)
      Exit.exitFailure
    Right text -> unless (Text.null text) (TextIO.putStrLn text)
