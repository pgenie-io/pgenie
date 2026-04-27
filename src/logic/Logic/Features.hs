module Logic.Features (spec) where

import qualified Logic.Features.CustomTypeSignatureFile as CustomTypeSignatureFile
import qualified Logic.Features.IndexOptimizer as IndexOptimizer
import qualified Logic.Features.Name as Name
import qualified Logic.Features.ProjectFile as ProjectFile
import qualified Logic.Features.SeqScanDetector as SeqScanDetector
import qualified Logic.Features.SignatureFile as SignatureFile
import qualified Logic.Features.SqlTemplate as SqlTemplate
import qualified Logic.Features.SyntaxAnalyser as SyntaxAnalyser
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
