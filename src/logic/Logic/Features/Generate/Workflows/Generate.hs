module Logic.Features.Generate.Workflows.Generate where

import Data.Text qualified as Text
import Logic.Features.AnalyseProject.Workflows.AnalyseProject qualified as AnalyseProject
import Logic.Features.GeneratorRuntime.Workflows.GenerateCode qualified as GenerateCode
import Logic.Features.IndexOptimization.Types.SeqScanFinding (SeqScanFinding (..))
import Logic.Features.ProjectModel.Types.ProjectModel qualified as ProjectFile
import Logic.Features.Reporting.Port (Warns (..))
import Logic.Features.Reporting.Types.Report (Report (..))
import Logic.Features.Staging.Port (Stages (..))
import Utils.Prelude

type Port m = (AnalyseProject.Port m, GenerateCode.Port m)

data Params = Params
  { projectFile :: ProjectFile.ProjectFile,
    failOnSeqScans :: Bool
  }

data Result = Result
  { generatedArtifacts :: [GenerateCode.Artifact]
  }

run :: (Port m) => Params -> m Result
run params =
  stage "" 2 do
    analyseResult <-
      AnalyseProject.run AnalyseProject.Params {projectFile = params.projectFile}
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
    generatedArtifacts <-
      GenerateCode.run
        GenerateCode.Params
          { projectFile = params.projectFile,
            project = analyseResult.project
          }
        <&> (.artifacts)
    pure Result {generatedArtifacts}
