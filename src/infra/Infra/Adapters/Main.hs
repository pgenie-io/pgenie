module Infra.Adapters.Main
  ( Device,
    scope,
    run,
  )
where

import AlgebraicPath qualified as Path
import Base.Prelude
import Data.Text.IO qualified as Text
import Fx
import Infra.Adapters.Analyser qualified as Analyser
import Infra.Adapters.Display qualified as Display
import Logic qualified
import PGenieGen qualified as Gen
import System.Directory qualified as Directory
import TextBuilder qualified

data Device = Device
  { display :: Display.Device,
    analyser :: Analyser.Device
  }

run :: Fx Device Logic.Error () -> IO ()
run fx =
  fx
    & scoping scope
    & handleErr
      ( \err ->
          runTotalIO \_dev -> do
            Text.putStrLn
              $ from @TextBuilder
              $ mconcat
              $ [ "Error at location: ",
                  TextBuilder.intercalateMap " > " to err.path,
                  "\nMessage: ",
                  to err.message,
                  maybe "" (mappend "\nSuggestion: " . to) err.suggestion,
                  if null err.details
                    then ""
                    else
                      "\nDetails:\n"
                        <> TextBuilder.intercalateMap
                          "\n"
                          ( \(key, value) ->
                              "  " <> to key <> ": " <> to value
                          )
                          err.details
                ]
      )
    & Fx.runFx

scope :: Fx.Scope Logic.Error Device
scope = do
  display <- Display.scope
  analyser <- Analyser.scope
  pure Device {display, analyser}

instance Logic.Reports (Fx Device Logic.Error) where
  enterStage path =
    Logic.enterStage path
      & mapEnv (.display)

  exitStage path progress =
    Logic.exitStage path progress
      & mapEnv (.display)

instance Logic.DbOps (Fx Device Logic.Error) where
  executeMigration migrationText =
    Logic.executeMigration migrationText
      & mapEnv (.analyser)

  inferQueryTypes queryText =
    Logic.inferQueryTypes queryText
      & mapEnv (.analyser)

instance Logic.FsOps (Fx Device Logic.Error) where
  readFile path =
    liftFileOp "Failed to read file" path do
      Text.readFile (Path.toFilePath path)

  writeFile path content =
    liftFileOp "Failed to write file" path do
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
  loadGen location =
    runExceptionalIO (const (Gen.load location))
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
