-- |
-- Root of the logic layer. Aggregates the test suites of its components for
-- the @logic-test@ test suite entry point.
module Logic (spec) where

import Logic.Domain qualified as Domain
import Logic.Procedures.GenerateCode qualified as GenerateCode
import Test.Hspec

-- | Test suite aggregating all logic-layer domain and procedure specs.
spec :: Spec
spec = do
  describe "Domain" Domain.spec
  describe "Procedures.GenerateCode" GenerateCode.spec
