module GenContractVersioningSpec (spec) where

import Dhall qualified
import GenContractVersioning (Codec (..), ContractVersion (..), DispatchError (..), codecByVersion)
import Mock.MockV3 (MockV3)
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "codecByVersion" do
    context "exact match on the latest rung" do
      it "accepts the exact declared version and round-trips through Dhall unmodified" do
        case codecByVersion @MockV3 (ContractVersion 3 1) of
          Left err -> expectationFailure ("Expected a codec, got: " <> show err)
          Right Codec {encode, decode} ->
            case encode 42 of
              Left err -> expectationFailure ("encode failed: " <> show err)
              Right expr -> decode expr `shouldBe` Right 42

    context "minor-ceiling" do
      it "accepts a declared minor at or below what this build implements" do
        case codecByVersion @MockV3 (ContractVersion 3 0) of
          Left err -> expectationFailure ("Expected a codec, got: " <> show err)
          Right _ -> pure ()

      it "rejects a declared minor above what this build implements" do
        case codecByVersion @MockV3 (ContractVersion 3 2) of
          Left err -> err `shouldBe` IncompatibleMinorDispatchError {requestedMinor = 2, implementedMinor = 1}
          Right _ -> expectationFailure "Expected the minor ceiling to reject this request"

    context "single-hop walk to the previous rung" do
      it "composes downgradeInput/upgradeOutput exactly once" do
        case codecByVersion @MockV3 (ContractVersion 2 0) of
          Left err -> expectationFailure ("Expected a codec, got: " <> show err)
          Right Codec {encode, decode} ->
            case encode 500 of
              Left err -> expectationFailure ("encode failed: " <> show err)
              Right expr -> do
                -- MockV3's downgradeInput subtracts 100; MockV2 is the
                -- terminal match here, so the encoded value is 500 - 100.
                Dhall.rawInput Dhall.auto expr `shouldBe` Just (400 :: Int)
                -- upgradeOutput adds 1000 on the way back up, exactly once.
                decode expr `shouldBe` Right 1400

    context "multi-hop walk to the oldest rung" do
      it "composes downgradeInput/upgradeOutput across every hop" do
        case codecByVersion @MockV3 (ContractVersion 1 0) of
          Left err -> expectationFailure ("Expected a codec, got: " <> show err)
          Right Codec {encode, decode} ->
            case encode 500 of
              Left err -> expectationFailure ("encode failed: " <> show err)
              Right expr -> do
                -- 500 - 100 (MockV3 hop) - 10 (MockV2 hop) = 390
                Dhall.rawInput Dhall.auto expr `shouldBe` Just (390 :: Int)
                -- Decoding that same 390 back up: +100 (MockV2 hop) +1000 (MockV3 hop) = 1490
                decode expr `shouldBe` Right 1490

    context "unsupported major" do
      it "fails once no rung in the chain implements the requested major" do
        case codecByVersion @MockV3 (ContractVersion 0 0) of
          Left err -> err `shouldBe` UnsupportedMajorDispatchError {requestedMajor = 0, oldestSupportedMajor = 1}
          Right _ -> expectationFailure "Expected no rung to accept major 0"

    context "a failing downgradeInput mid-chain" do
      it "surfaces the rung's own error instead of silently succeeding" do
        case codecByVersion @MockV3 (ContractVersion 2 0) of
          Left err -> expectationFailure ("Expected a codec, got: " <> show err)
          Right Codec {encode} ->
            encode (-1) `shouldBe` Left "MockV3: negative inputs cannot be represented in the older contract"
