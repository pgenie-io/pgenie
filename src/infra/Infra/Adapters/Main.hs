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
import Logic.Features.Fs.Port (FsOps (..))
import Logic.Features.GeneratorRuntime.Port (LoadsGen (..))
import Logic.Features.IndexCatalog.Port (LoadsIndexes (..))
import Logic.Features.Migrations.Port (ExecutesMigrations (..))
import Logic.Features.ProjectModel.Types.ProjectModel qualified as ProjectFile
import Logic.Features.QueryAnalysis.Port (InfersQueryTypes (..))
import Logic.Features.Reporting.Types.Report qualified as Report
import Logic.Features.SeqScanExplain.Port (ExplainsQuery (..))
import PGenieGen qualified as Gen
import System.Directory qualified as Directory
import System.Info qualified as Info
import Utils.Prelude

data Device = Device
  { observe :: Observing.Observation -> IO (),
    analyser :: Analyser.Device,
    projectFile :: ProjectFile.ProjectFile
  }

getProjectFile :: Fx Device Report.Report ProjectFile.ProjectFile
getProjectFile =
  runTotalIO \dev -> pure dev.projectFile

scope :: (Observing.Observation -> IO ()) -> Maybe Text -> Fx.Scope Report.Report Device
scope observe maybeDatabaseUrl = do
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
    runExceptionalIO (const (Gen.load location maybeHash (const (pure ()))))
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
