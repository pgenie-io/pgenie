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
import System.Directory qualified as Directory
import Utils.Prelude

data Device = Device
  { emitEvent :: Logic.Event -> IO (),
    analyser :: Analyser.Device,
    projectFile :: ProjectFile.ProjectFile
  }

scope :: (Logic.Event -> IO ()) -> Fx.Scope Logic.Error Device
scope emitEvent = do
  let halveEvent = \case
        Logic.StageExited path progress ->
          Logic.StageExited path (progress / 2)
        otherEvent ->
          otherEvent
      halvedEmitEvent =
        emitEvent . halveEvent

  projectFile <-
    let path = "project1.pgn.yaml"
     in acquire do
          liftFileOp
            "Failed to load project file"
            path
            (Text.readFile (Path.toFilePath path))

  projectFile <- ProjectFile.tryFromYaml projectFile

  let postgresTag = case projectFile.postgres of
        Nothing -> "postgres:18"
        Just postgres -> "postgres:" <> Text.pack (show postgres)

  analyser <- Analyser.scope postgresTag halvedEmitEvent
  pure
    Device
      { emitEvent = halvedEmitEvent,
        analyser,
        projectFile
      }

instance Logic.Emits (Fx Device Logic.Error) where
  emit event =
    runTotalIO \dev -> dev.emitEvent event

instance Logic.DbOps (Fx Device Logic.Error) where
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

instance Logic.FsOps (Fx Device Logic.Error) where
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
            Logic.Error
              { path = [],
                message = "Invalid file path",
                suggestion = Nothing,
                details =
                  [ ("input", onto filePath)
                  ]
              }

instance Logic.LoadsGen (Fx Device Logic.Error) where
  loadGen location maybeHash =
    runExceptionalIO (const (Gen.load location maybeHash (const (pure ()))))
      & first
        ( \err ->
            Logic.Error
              { path = [],
                message = "Failed to load gen",
                suggestion = Nothing,
                details =
                  [ ("reason", onto (displayException @SomeException err))
                  ]
              }
        )

instance Logic.LoadsProjectFile (Fx Device Logic.Error) where
  loadProjectFile =
    runTotalIO \dev -> pure dev.projectFile

liftFileOp :: Text -> Path -> IO a -> Fx env Logic.Error a
liftFileOp errMessage path action =
  runExceptionalIO (const action)
    & first
      ( \err ->
          Logic.Error
            { path = [],
              message = errMessage,
              suggestion = Nothing,
              details =
                [ ("reason", onto (displayException @IOException err)),
                  ("path", Path.toText path)
                ]
            }
      )
