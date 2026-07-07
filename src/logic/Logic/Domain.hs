module Logic.Domain (spec) where

import Logic.Domain.CustomTypeSignature qualified as CustomTypeSignatureFile
import Logic.Domain.IndexOptimization qualified as IndexOptimizer
import Logic.Domain.Name qualified as Name
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.QuerySignature qualified as SignatureFile
import Logic.Domain.SeqScanFinding qualified as SeqScanDetector
import Logic.Domain.SqlTemplate qualified as SqlTemplate
import Logic.Domain.SyntaxAnalyser qualified as SyntaxAnalyser
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
