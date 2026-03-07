module EmbeddingsSpec (spec) where

import Base.Prelude
import Infra.Adapters.Analyser.Embeddings.Sessions
import Infra.Adapters.Analyser.Sessions.Domain qualified as Domain
import Logic.Algebra qualified as Logic
import PGenieGen.Model.Input qualified as Gen.Input
import Test.Hspec

spec :: Spec
spec = do
  describe "adaptType" do
    it "sets elementIsNullable to True for array types" do
      let arrayType =
            Domain.Type
              { dimensionality = 1,
                scalar = Domain.PrimitiveScalar Domain.Int4Primitive
              }
      case adaptType arrayType of
        Left Logic.Error {message} -> expectationFailure $ "Expected Right but got Left: " <> show message
        Right Gen.Input.Value {arraySettings = Nothing} -> expectationFailure "Expected array settings but got Nothing"
        Right Gen.Input.Value {arraySettings = Just Gen.Input.ArraySettings {elementIsNullable}} ->
          elementIsNullable `shouldBe` True

    it "does not set array settings for non-array types" do
      let scalarType =
            Domain.Type
              { dimensionality = 0,
                scalar = Domain.PrimitiveScalar Domain.Int4Primitive
              }
      case adaptType scalarType of
        Left Logic.Error {message} -> expectationFailure $ "Expected Right but got Left: " <> show message
        Right Gen.Input.Value {arraySettings} -> arraySettings `shouldBe` Nothing
