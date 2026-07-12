-- |
-- Entry point wiring the application's infrastructure adapters to a
-- caller-supplied program and running it to completion.
module Infra
  ( run,
  )
where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Fx
import Infra.Adapters.Main qualified as MainAdapter
import Interpreters.Observing qualified as Observing
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.Report qualified as Report
import System.Exit qualified as Exit
import Utils.Prelude

-- |
-- Scope the infrastructure device, run the given program against it, and
-- print its result or report and exit with failure on error.
run :: (Observing.Observation -> IO ()) -> Maybe Text -> Bool -> (ProjectFile.ProjectFile -> Observing.Observing (Fx MainAdapter.Device Report.Report) Text) -> IO ()
run observe maybeDatabaseUrl reuse makeFx = do
  result <-
    ( do
        projectFile <- MainAdapter.getProjectFile
        Observing.interpretObserving (makeFx projectFile)
    )
      & scoping (MainAdapter.scope observe maybeDatabaseUrl reuse)
      & exposeErr
      & Fx.runFx
  case result of
    Left err -> do
      observe (Observing.ExecutionFailed err)
      Exit.exitFailure
    Right text -> unless (Text.null text) (Text.IO.putStrLn text)
