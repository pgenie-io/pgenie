module EmbeddingsSpec (spec) where

import Base.Prelude
import Infra.Adapters.Analyser.Embeddings.Sessions
import Infra.Adapters.Analyser.Sessions.Domain qualified as Domain
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
        Left err -> expectationFailure $ "Expected Right but got Left: " <> to err.message
        Right value -> case value.arraySettings of
          Nothing -> expectationFailure "Expected array settings but got Nothing"
          Just settings -> settings.elementIsNullable `shouldBe` True

    it "does not set array settings for non-array types" do
      let scalarType =
            Domain.Type
              { dimensionality = 0,
                scalar = Domain.PrimitiveScalar Domain.Int4Primitive
              }
      case adaptType scalarType of
        Left err -> expectationFailure $ "Expected Right but got Left: " <> to err.message
        Right value -> value.arraySettings `shouldBe` Nothing
