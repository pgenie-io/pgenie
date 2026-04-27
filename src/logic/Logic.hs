module Logic (spec) where

import qualified Logic.Features as Features
import qualified Logic.Workflows as Workflows
import Test.Hspec

spec :: Spec
spec = do
  describe "Features" Features.spec
  describe "Workflows" Workflows.spec
