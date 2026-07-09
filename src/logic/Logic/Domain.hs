-- |
-- Aggregates the domain types of the logic layer: names, project files,
-- query and custom-type signatures, index optimization, seq-scan findings,
-- SQL templates, and SQL syntax analysis.
module Logic.Domain (spec) where

import Logic.Domain.CustomTypeSignature qualified as CustomTypeSignature
import Logic.Domain.IndexOptimization qualified as IndexOptimization
import Logic.Domain.Name qualified as Name
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.QuerySignature qualified as QuerySignature
import Logic.Domain.SeqScanFinding qualified as SeqScanFinding
import Logic.Domain.SqlTemplate qualified as SqlTemplate
import Logic.Domain.SyntaxAnalyser qualified as SyntaxAnalyser
import Test.Hspec

-- | Test suite aggregating the specs of all domain modules.
spec :: Spec
spec = do
  describe "CustomTypeSignature" CustomTypeSignature.spec
  describe "IndexOptimization" IndexOptimization.spec
  describe "Name" Name.spec
  describe "ProjectFile" ProjectFile.spec
  describe "SeqScanFinding" SeqScanFinding.spec
  describe "QuerySignature" QuerySignature.spec
  describe "SqlTemplate" SqlTemplate.spec
  describe "SyntaxAnalyser" SyntaxAnalyser.spec
