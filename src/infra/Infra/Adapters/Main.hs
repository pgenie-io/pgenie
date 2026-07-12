-- |
-- Top-level infrastructure device: wires the project file, the observer, and
-- the 'Analyser.Device' together, and implements the application's logic
-- Ports (migrations, query analysis, filesystem, gen loading) atop them.
module Infra.Adapters.Main
  ( Device,
    getProjectFile,
    scope,
  )
where

import AlgebraicPath qualified as Path
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Fx
import GenBridge qualified as Gen
import Infra.Adapters.Analyser qualified as Analyser
import Interpreters.Observing qualified as Observing
import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.GeneratorRuntime (LoadsGen (..))
import Logic.Capabilities.IndexCatalog (LoadsIndexes (..))
import Logic.Capabilities.Migrations (ExecutesMigrations (..))
import Logic.Capabilities.QueryAnalysis (InfersQueryTypes (..))
import Logic.Capabilities.SeqScanExplain (ExplainsQuery (..))
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.Report qualified as Report
import System.Directory qualified as Directory
import System.Info qualified as Info
import Utils.Prelude

-- | The application's top-level infrastructure device.
data Device = Device
  { observe :: Observing.Observation -> IO (),
    analyser :: Analyser.Device,
    projectFile :: ProjectFile.ProjectFile
  }

-- | The project file loaded when the device was scoped.
getProjectFile :: Fx Device Report.Report ProjectFile.ProjectFile
getProjectFile =
  runTotalIO \dev -> pure dev.projectFile

-- |
-- Load the project file, scope the analyser (halving its reported progress
-- to leave room for the rest of the run), and assemble the 'Device'. Fails
-- fast on Windows when no database URL is given, since Docker-based analysis
-- isn't supported there yet. @reuse@ is only meaningful in Docker mode
-- (i.e. when no database URL is given) and is ignored otherwise.
scope :: (Observing.Observation -> IO ()) -> Maybe Text -> Bool -> Fx.Scope Report.Report Device
scope observe maybeDatabaseUrl reuse = do
  -- Terminate early on Windows in Docker mode since it's not supported yet.
  when (isNothing maybeDatabaseUrl && Info.os == "mingw32") do
    throwError
      Report.Report
        { path = [],
          message = "Docker execution mode on Windows is under development. Windows users can only use the live Postgres mode for now.",
          suggestion = Just "Run pgn with --database-url to connect to a running PostgreSQL server. See https://pgenie.io/docs/guides/live-instance-mode for more details.",
          details = []
        }

  projectFile <-
    let path = "project1.pgn.yaml"
     in acquire do
          liftFileOp "Failed to load project file" path do
            Text.IO.readFile (Path.toFilePath path)

  projectFile <- ProjectFile.tryFromYaml projectFile

  let targetMajorVersion = fromMaybe 18 projectFile.postgres
      source = case maybeDatabaseUrl of
        Nothing ->
          let postgresTag = "postgres:" <> Text.pack (show targetMajorVersion)
           in Analyser.DockerSource {postgresTag, reuse}
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

instance Observing.Observes (Fx Device Report.Report) where
  observe observation =
    runTotalIO \dev -> dev.observe observation

instance ExecutesMigrations (Fx Device Report.Report) where
  executeMigration migrationText =
    executeMigration migrationText
      & mapEnv (.analyser)

instance InfersQueryTypes (Fx Device Report.Report) where
  inferQueryTypes queryText =
    inferQueryTypes queryText
      & mapEnv (.analyser)

instance ExplainsQuery (Fx Device Report.Report) where
  explainQuery sql =
    explainQuery sql
      & mapEnv (.analyser)

instance LoadsIndexes (Fx Device Report.Report) where
  getIndexes =
    getIndexes
      & mapEnv (.analyser)

instance FsOps (Fx Device Report.Report) where
  readFile path =
    liftFileOp "Failed to read file" path do
      Text.IO.readFile (Path.toFilePath path)

  writeFile path content =
    liftFileOp "Failed to write file" path do
      Directory.createDirectoryIfMissing True (Path.toFilePath (path <> ".."))
      Text.IO.writeFile (Path.toFilePath path) content

  listDir path = do
    filePaths <- liftFileOp "Failed to list directory" path do
      Directory.listDirectory (Path.toFilePath path)
    forM filePaths \filePath -> do
      case Path.maybeFromFilePath filePath of
        Just path' ->
          pure path'
        Nothing ->
          throwError
            Report.Report
              { path = [],
                message = "Invalid file path",
                suggestion = Nothing,
                details =
                  [ ("input", onto filePath)
                  ]
              }

instance LoadsGen (Fx Device Report.Report) where
  loadGen location maybeHash =
    runExceptionalIO
      ( \dev ->
          let logInfo _ = pure ()
              logWarning msg =
                let report =
                      Report.Report
                        { path = [],
                          message = msg,
                          suggestion = Nothing,
                          details =
                            [ ( "location",
                                case location of
                                  Gen.LocationUrl url -> onto url
                                  Gen.LocationPath path -> onto (Path.toText path)
                              )
                            ]
                        }
                 in dev.observe (Observing.WarningEmitted report)
           in Gen.load location maybeHash logInfo logWarning
      )
      & first
        ( \err ->
            Report.Report
              { path = [],
                message = "Failed to load gen",
                suggestion = Just "Update to the latest releases of pgn and codegen",
                details =
                  [ ( "reason",
                      onto (displayException @SomeException err)
                    ),
                    ( "location",
                      case location of
                        Gen.LocationUrl url -> onto url
                        Gen.LocationPath path -> onto (Path.toText path)
                    )
                  ]
              }
        )

liftFileOp :: Text -> Path -> IO a -> Fx env Report.Report a
liftFileOp errMessage path action =
  runExceptionalIO (const action)
    & first
      ( \err ->
          Report.Report
            { path = [],
              message = errMessage,
              suggestion = Nothing,
              details =
                [ ("reason", onto (displayException @IOException err)),
                  ("path", Path.toText path)
                ]
            }
      )
