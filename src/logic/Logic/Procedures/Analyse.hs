module Logic.Procedures.Analyse where

import Data.Aeson.Text qualified as Aeson.Text
import Data.Text qualified as Text
import Dhall.Core qualified as Dhall
import Dhall.Marshal.Encode qualified as Dhall
import Logic.Capabilities.Reporting (Warns (..))
import Logic.Capabilities.Staging (Stages (..))
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.Report (Report (..))
import Logic.Domain.SeqScanFinding (SeqScanFinding (..))
import Logic.Procedures.AnalyseProject qualified as AnalyseProject
import PGenieGen.Model.Aeson ()
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
                  ( if null finding.suggestedIndexColumns
                      then Nothing
                      else Just "Run 'manage-indexes' to generate index migration"
                  )
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
