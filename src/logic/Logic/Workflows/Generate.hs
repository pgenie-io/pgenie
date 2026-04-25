module Logic.Workflows.Generate where

import Data.Text qualified as Text
import Logic.Features.ProjectFile qualified as ProjectFile
import Logic.Features.Report (Report (..), Warns (..))
import Logic.Features.SeqScanDetector (SeqScanFinding (..))
import Logic.Features.Staging (Stages (..))
import Logic.Workflows.AnalyseProject qualified as AnalyseProject
import Logic.Workflows.GenerateCode qualified as GenerateCode
import Utils.Prelude

type Port m = (AnalyseProject.Port m, GenerateCode.Port m)

data Params = Params
  { failOnSeqScans :: Bool
  }
  deriving stock (Eq, Show)

run :: (Port m) => ProjectFile.ProjectFile -> Params -> m ()
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
    GenerateCode.run projectFile analyseResult.project
    pure ()
