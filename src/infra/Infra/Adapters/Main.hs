module Infra.Adapters.Main
  ( Device,
    scope,
  )
where

import AlgebraicPath qualified as Path
import Data.Text.IO qualified as Text
import Fx
import Infra.Adapters.Analyser qualified as Analyser
import Logic qualified
import PGenieGen qualified as Gen
import System.Directory qualified as Directory
import Utils.Prelude

data Device = Device
  { emitEvent :: Logic.Event -> IO (),
    analyser :: Analyser.Device
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
  analyser <- Analyser.scope halvedEmitEvent
  pure
    Device
      { emitEvent = halvedEmitEvent,
        analyser
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

liftFileOp :: Text -> Path -> IO a -> Fx Device Logic.Error a
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
