module Logic.Features.Analyse.Workflows.Analyse where

import Data.Aeson.Text qualified as Aeson.Text
import Data.Text qualified as Text
import Dhall.Core qualified as Dhall
import Dhall.Marshal.Encode qualified as Dhall
import Logic.Features.AnalyseProject.Workflows.AnalyseProject qualified as AnalyseProject
import Logic.Features.IndexOptimization.Types.SeqScanFinding (SeqScanFinding (..))
import Logic.Features.ProjectModel.Types.ProjectModel qualified as ProjectFile
import Logic.Features.Reporting.Port (Warns (..))
import Logic.Features.Reporting.Types.Report (Report (..))
import Logic.Features.Staging.Port (Stages (..))
import Utils.Prelude

type Port m = AnalyseProject.Port m

data Params = Params
  { projectFile :: ProjectFile.ProjectFile,
    failOnSeqScans :: Bool,
    output :: Maybe ModelFormat
  }

data ModelFormat = ModelFormatJson | ModelFormatDhall
  deriving stock (Eq, Show)

data Result = Result
  { outputText :: Text
  }

run :: (Port m) => Params -> m Result
run params =
  stage "" 2 do
    analyseResult <-
      AnalyseProject.run AnalyseProject.Params {projectFile = params.projectFile}
    unless (null analyseResult.seqScanFindings) do
      if params.failOnSeqScans
        then
          throwError
            ( Report
                []
                "Sequential scans detected"
                (Just "Run 'manage-indexes' to generate index migration, or remove --fail-on-seq-scans to allow warnings")
                [ ("queries", Text.intercalate ", " (map fst analyseResult.seqScanFindings))
                ]
            )
        else do
          for_ analyseResult.seqScanFindings \(queryName, finding) ->
            warn
              ( Report
                  []
                  "Sequential scan detected"
                  (Just "Run 'manage-indexes' to generate index migration")
                  [ ("query", queryName),
                    ("table", finding.tableName),
                    ("columns", Text.intercalate ", " finding.suggestedIndexColumns)
                  ]
              )
    case params.output of
      Nothing -> pure Result {outputText = ""}
      Just ModelFormatDhall ->
        pure
          Result
            { outputText =
                Dhall.pretty (Dhall.cse (Dhall.denote (Dhall.inject.embed analyseResult.project)))
            }
      Just ModelFormatJson ->
        pure
          Result
            { outputText = to (Aeson.Text.encodeToTextBuilder analyseResult.project)
            }
