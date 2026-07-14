module GenContractVersioningSpec (spec) where

import GenContractVersioning
import Test.Hspec
import Prelude

-- Three throwaway mock tags forming Grandparent <- Parent <- Child, fully
-- decoupled from any real gen-contract rung. MockParent's downgrade can
-- genuinely fail (negative counts aren't representable one rung down) so
-- the Either short-circuiting has something real to prove.

data MockChild

data MockParent

data MockGrandparent

instance IsContract MockGrandparent where
  type InputOf MockGrandparent = Int
  type OutputOf MockGrandparent = String

instance IsContract MockParent where
  type InputOf MockParent = Int
  type OutputOf MockParent = String

instance IsContract MockChild where
  type InputOf MockChild = Int
  type OutputOf MockChild = String

instance HasParent MockParent where
  type ParentOf MockParent = MockGrandparent

  downgradeInput n
    | n <= 0 = Left "MockParent can't downgrade a negative count"
    | otherwise = Right (n - 1)

  upgradeOutput = ("grandparent:" <>)

instance HasParent MockChild where
  type ParentOf MockChild = MockParent

  downgradeInput n
    | n < 0 = Left "MockChild can't downgrade a negative count"
    | otherwise = Right (n * 2)

  upgradeOutput = ("parent:" <>)

spec :: Spec
spec = do
  describe "downgradeInput composed across two hops" do
    it "threads a successful value all the way to the grandparent rung" do
      (downgradeInput @MockChild 5 >>= downgradeInput @MockParent)
        `shouldBe` Right 9

    it "short-circuits on the first failing hop without calling the second" do
      (downgradeInput @MockChild (-5) >>= downgradeInput @MockParent)
        `shouldBe` Left "MockChild can't downgrade a negative count"

    it "short-circuits on the second hop when only it fails" do
      (downgradeInput @MockChild 0 >>= downgradeInput @MockParent)
        `shouldBe` Left "MockParent can't downgrade a negative count"

  describe "upgradeOutput composed across two hops, in reverse order" do
    it "lifts a grandparent-rung output all the way up to the child rung" do
      (upgradeOutput @MockChild . upgradeOutput @MockParent) "ok"
        `shouldBe` "parent:grandparent:ok"
