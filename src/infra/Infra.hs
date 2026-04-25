module Infra
  ( run,
  )
where

import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Fx
import Infra.Adapters.Main qualified as MainAdapter
import Interpreters.Observing qualified as Observing
import Logic qualified
import Logic.ProjectFile qualified as ProjectFile
import System.Exit qualified as Exit
import Utils.Prelude

run :: (Observing.Observation -> IO ()) -> Maybe Text -> (ProjectFile.ProjectFile -> Observing.Observing (Fx MainAdapter.Device Logic.Report) Text) -> IO ()
run observe maybeDatabaseUrl makeFx = do
  result <-
    ( do
        projectFile <- MainAdapter.getProjectFile
        Observing.interpretObserving (makeFx projectFile)
    )
      & scoping (MainAdapter.scope observe maybeDatabaseUrl)
      & exposeErr
      & Fx.runFx
  case result of
    Left err -> do
      observe (Observing.ExecutionFailed err)
      Exit.exitFailure
    Right text -> unless (Text.null text) (TextIO.putStrLn text)
