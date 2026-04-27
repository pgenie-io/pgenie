module Logic.Features (spec) where

import Logic.Features.CustomTypeSignatures.Types.CustomTypeSignatures qualified as CustomTypeSignatureFile
import Logic.Features.IndexOptimization.Types.IndexOptimization qualified as IndexOptimizer
import Logic.Features.IndexOptimization.Types.SeqScanFinding qualified as SeqScanDetector
import Logic.Features.Naming.Types.Name qualified as Name
import Logic.Features.ProjectModel.Types.ProjectModel qualified as ProjectFile
import Logic.Features.QuerySignatures.Types.QuerySignatures qualified as SignatureFile
import Logic.Features.SqlTemplates.Types.SqlTemplates qualified as SqlTemplate
import Logic.Features.SyntaxAnalyser qualified as SyntaxAnalyser
import Test.Hspec

spec :: Spec
spec = do
  describe "CustomTypeSignatureFile" CustomTypeSignatureFile.spec
  describe "IndexOptimizer" IndexOptimizer.spec
  describe "Name" Name.spec
  describe "ProjectFile" ProjectFile.spec
  describe "SeqScanDetector" SeqScanDetector.spec
  describe "SignatureFile" SignatureFile.spec
  describe "SqlTemplate" SqlTemplate.spec
  describe "SyntaxAnalyser" SyntaxAnalyser.spec
