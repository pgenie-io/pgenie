module Logic (spec) where

import Logic.Domain qualified as Domain
import Logic.Procedures.GenerateCode qualified as GenerateCode
import Test.Hspec

spec :: Spec
spec = do
  describe "Domain" Domain.spec
  describe "Procedures.GenerateCode" GenerateCode.spec
