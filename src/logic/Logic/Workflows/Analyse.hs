module Logic.Workflows.Analyse where

import Data.Aeson.Text qualified as Aeson.Text
import Data.Text qualified as Text
import Dhall.Core qualified as Dhall
import Dhall.Marshal.Encode qualified as Dhall
import Logic.Features.ProjectFile qualified as ProjectFile
import Logic.Features.Report (Report (..), Warns (..))
import Logic.Features.SeqScanDetector (SeqScanFinding (..))
import Logic.Features.Staging (Stages (..))
import Logic.Workflows.AnalyseProject qualified as AnalyseProject
import Utils.Prelude

type Port m = AnalyseProject.Port m

data Params = Params
  { failOnSeqScans :: Bool,
    output :: Maybe ModelFormat
  }
  deriving stock (Eq, Show)

data ModelFormat = ModelFormatJson | ModelFormatDhall
  deriving stock (Eq, Show)

type Result = Text

run :: (Port m) => ProjectFile.ProjectFile -> Params -> m Result
run projectFile params =
  stage "" 2 do
    analyseResult <- AnalyseProject.run projectFile
    unless (null analyseResult.seqScanFindings) do
      for_ analyseResult.seqScanFindings \(queryName, finding) ->
        warn
          ( Report
              []
              ( "Sequential scan detected in query '"
                  <> queryName
                  <> "': table '"
                  <> finding.tableName
                  <> "' scanned without index on ("
                  <> Text.intercalate ", " finding.suggestedIndexColumns
                  <> ")"
              )
              (Just "Run 'manage-indexes' to generate index migration")
              [("query", queryName), ("table", finding.tableName)]
          )
      when params.failOnSeqScans do
        throwError
          ( Report
              []
              "Sequential scans detected"
              (Just "Run 'manage-indexes' to generate index migration, or remove --fail-on-seq-scans to allow warnings")
              []
          )
    case params.output of
      Nothing -> pure ""
      Just ModelFormatDhall -> pure (Dhall.pretty (Dhall.cse (Dhall.denote (Dhall.inject.embed analyseResult.project))))
      Just ModelFormatJson -> pure (to (Aeson.Text.encodeToTextBuilder analyseResult.project))
