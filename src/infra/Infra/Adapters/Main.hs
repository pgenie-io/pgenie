module Infra.Adapters.Main
  ( Device,
    scope,
  )
where

import AlgebraicPath qualified as Path
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Fx
import Infra.Adapters.Analyser qualified as Analyser
import Logic qualified
import Logic.ProjectFile qualified as ProjectFile
import PGenieGen qualified as Gen
import Runtime.Emitting qualified as Emitting
import Runtime.Observation qualified as Observation
import System.Directory qualified as Directory
import System.Info qualified as Info
import Utils.Prelude

data Device = Device
  { emitObservation :: Observation.Observation -> IO (),
    analyser :: Analyser.Device,
    projectFile :: ProjectFile.ProjectFile
  }

scope :: (Observation.Observation -> IO ()) -> Maybe Text -> Fx.Scope Logic.Report Device
scope emitObservation maybeDatabaseUrl = do
  -- Terminate early on Windows in Docker mode since it's not supported yet.
  when (isNothing maybeDatabaseUrl && Info.os == "mingw32") do
    throwError
      Logic.Report
        { path = [],
          message = "Docker execution mode on Windows is under development. Windows users can only use the live Postgres mode for now.",
          suggestion = Just "Run pgn with --database-url to connect to a running PostgreSQL server. See https://pgenie.io/docs/guides/live-instance-mode for more details.",
          details = []
        }

  projectFile <-
    let path = "project1.pgn.yaml"
     in acquire do
          liftFileOp "Failed to load project file" path do
            Text.readFile (Path.toFilePath path)

  projectFile <- ProjectFile.tryFromYaml projectFile

  let targetMajorVersion = fromMaybe 18 projectFile.postgres
      source = case maybeDatabaseUrl of
        Nothing ->
          let postgresTag = "postgres:" <> Text.pack (show targetMajorVersion)
           in Analyser.DockerSource {postgresTag}
        Just url ->
          Analyser.RunningServerSource {connectionUrl = url, targetMajorVersion}

  let halveObservation = \case
        Observation.StageExited path progress ->
          Observation.StageExited path (progress / 2)
        otherObservation ->
          otherObservation
      halvedEmitObservation =
        emitObservation . halveObservation

  analyser <- Analyser.scope source halvedEmitObservation
  pure
    Device
      { emitObservation = halvedEmitObservation,
        analyser,
        projectFile
      }

instance Emitting.EmitsObservation (Fx Device Logic.Report) where
  emitObservation observation =
    runTotalIO \dev -> dev.emitObservation observation

instance Logic.DbOps (Fx Device Logic.Report) where
  executeMigration migrationText =
    Logic.executeMigration migrationText
      & mapEnv (.analyser)

  inferQueryTypes queryText =
    Logic.inferQueryTypes queryText
      & mapEnv (.analyser)

  explainQuery sql =
    Logic.explainQuery sql
      & mapEnv (.analyser)

  getIndexes =
    Logic.getIndexes
      & mapEnv (.analyser)

instance Logic.FsOps (Fx Device Logic.Report) where
  readFile path =
    liftFileOp "Failed to read file" path do
      Text.readFile (Path.toFilePath path)

  writeFile path content =
    liftFileOp "Failed to write file" path do
      Directory.createDirectoryIfMissing True (Path.toFilePath (path <> ".."))
      Text.writeFile (Path.toFilePath path) content

  listDir path = do
    filePaths <- liftFileOp "Failed to list directory" path do
      Directory.listDirectory (Path.toFilePath path)
    forM filePaths \filePath -> do
      case Path.maybeFromFilePath filePath of
        Just path' ->
          pure path'
        Nothing ->
          throwError
            Logic.Report
              { path = [],
                message = "Invalid file path",
                suggestion = Nothing,
                details =
                  [ ("input", onto filePath)
                  ]
              }

instance Logic.LoadsGen (Fx Device Logic.Report) where
  loadGen location maybeHash =
    runExceptionalIO (const (Gen.load location maybeHash (const (pure ()))))
      & first
        ( \err ->
            Logic.Report
              { path = [],
                message = "Failed to load gen",
                suggestion = Nothing,
                details =
                  [ ("reason", onto (displayException @SomeException err))
                  ]
              }
        )

instance Logic.LoadsProjectFile (Fx Device Logic.Report) where
  loadProjectFile =
    runTotalIO \dev -> pure dev.projectFile

liftFileOp :: Text -> Path -> IO a -> Fx env Logic.Report a
liftFileOp errMessage path action =
  runExceptionalIO (const action)
    & first
      ( \err ->
          Logic.Report
            { path = [],
              message = errMessage,
              suggestion = Nothing,
              details =
                [ ("reason", onto (displayException @IOException err)),
                  ("path", Path.toText path)
                ]
            }
      )
