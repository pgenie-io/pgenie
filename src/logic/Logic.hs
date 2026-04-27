module Logic (spec) where

import Logic.Features qualified as Features
import Test.Hspec

spec :: Spec
spec = do
  describe "Features" Features.spec
