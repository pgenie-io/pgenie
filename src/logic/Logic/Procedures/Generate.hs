module Logic.Procedures.Generate where

import Data.Text qualified as Text
import Logic.Capabilities.Reporting (Warns (..))
import Logic.Capabilities.Staging (Stages (..))
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.Report (Report (..))
import Logic.Domain.SeqScanFinding (SeqScanFinding (..))
import Logic.Procedures.AnalyseProject qualified as AnalyseProject
import Logic.Procedures.GenerateCode qualified as GenerateCode
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
    generatedArtifacts <-
      GenerateCode.run
        GenerateCode.Params
          { projectFile = params.projectFile,
            project = analyseResult.project
          }
        <&> (.artifacts)
    pure Result {generatedArtifacts}
