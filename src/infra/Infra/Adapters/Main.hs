module Infra.Adapters.Main
  ( Device,
    getProjectFile,
    scope,
  )
where

import AlgebraicPath qualified as Path
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Fx
import Infra.Adapters.Analyser qualified as Analyser
import Interpreters.Observing qualified as Observing
import Logic qualified
import Logic.Features.CustomTypeSignatureFile qualified as CustomTypeSignatureFile
import Logic.Features.GeneratorHashes qualified as GeneratorHashes
import Logic.Features.ProjectFile qualified as ProjectFile
import PGenieGen qualified as Gen
import System.Directory qualified as Directory
import System.Info qualified as Info
import Utils.Prelude

data Device = Device
  { observe :: Observing.Observation -> IO (),
    analyser :: Analyser.Device,
    projectFile :: ProjectFile.ProjectFile
  }

getProjectFile :: Fx Device Logic.Report ProjectFile.ProjectFile
getProjectFile =
  runTotalIO \dev -> pure dev.projectFile

scope :: (Observing.Observation -> IO ()) -> Maybe Text -> Fx.Scope Logic.Report Device
scope observe maybeDatabaseUrl = do
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

      halveObservation = \case
        Observing.StageExited path progress ->
          Observing.StageExited path (progress / 2)
        otherObservation ->
          otherObservation
      halvedObserve =
        observe . halveObservation

  analyser <- Analyser.scope source halvedObserve
  pure
    Device
      { observe = halvedObserve,
        analyser,
        projectFile
      }

instance Observing.Observes (Fx Device Logic.Report) where
  observe observation =
    runTotalIO \dev -> dev.observe observation

instance Logic.ExecutesMigrations (Fx Device Logic.Report) where
  executeMigration migrationText =
    Logic.executeMigration migrationText
      & mapEnv (.analyser)

instance Logic.InfersQueryTypes (Fx Device Logic.Report) where
  inferQueryTypes queryText =
    Logic.inferQueryTypes queryText
      & mapEnv (.analyser)

instance Logic.ExplainsQuery (Fx Device Logic.Report) where
  explainQuery sql =
    Logic.explainQuery sql
      & mapEnv (.analyser)

instance Logic.LoadsIndexes (Fx Device Logic.Report) where
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

instance CustomTypeSignatureFile.Port (Fx Device Logic.Report) where
  readFile path =
    liftFileOp "Failed to read custom-type signature file" path do
      Text.readFile (Path.toFilePath path)

  writeFile path content =
    liftFileOp "Failed to write custom-type signature file" path do
      Directory.createDirectoryIfMissing True (Path.toFilePath (path <> ".."))
      Text.writeFile (Path.toFilePath path) content

instance GeneratorHashes.Port (Fx Device Logic.Report) where
  readFile path =
    liftFileOp "Failed to read generator hashes file" path do
      Text.readFile (Path.toFilePath path)

  writeFile path content =
    liftFileOp "Failed to write generator hashes file" path do
      Directory.createDirectoryIfMissing True (Path.toFilePath (path <> ".."))
      Text.writeFile (Path.toFilePath path) content

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
