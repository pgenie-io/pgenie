module Logic (spec) where

import Logic.Domain qualified as Domain
import Test.Hspec

spec :: Spec
spec = do
  describe "Domain" Domain.spec
