module GenContractV5.Input.ValueSpec (spec) where

import GenContractV4.Input.ArraySettings qualified as V4.ArraySettings
import GenContractV4.Input.Primitive qualified as V4.Primitive
import GenContractV4.Input.Scalar qualified as V4.Scalar
import GenContractV4.Input.Value qualified as V4.Value
import GenContractV5.Input.Primitive qualified as V5.Primitive
import GenContractV5.Input.Scalar qualified as V5.Scalar
import GenContractV5.Input.Value qualified as V5.Value
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "toV4Value" do
    it "maps dimensionality = 0 to arraySettings = Nothing" do
      V5.Value.toV4Value
        V5.Value.Value
          { dimensionality = 0,
            elementIsNullable = True,
            scalar = V5.Scalar.PrimitiveScalar V5.Primitive.Int4Primitive
          }
        `shouldBe` V4.Value.Value
          { arraySettings = Nothing,
            scalar = V4.Scalar.PrimitiveScalar V4.Primitive.Int4Primitive
          }

    it "maps dimensionality > 0 to Just an ArraySettings carrying the same two fields" do
      V5.Value.toV4Value
        V5.Value.Value
          { dimensionality = 2,
            elementIsNullable = True,
            scalar = V5.Scalar.PrimitiveScalar V5.Primitive.TextPrimitive
          }
        `shouldBe` V4.Value.Value
          { arraySettings = Just V4.ArraySettings.ArraySettings {dimensionality = 2, elementIsNullable = True},
            scalar = V4.Scalar.PrimitiveScalar V4.Primitive.TextPrimitive
          }

    it "property: dimensionality = 0 always maps to Nothing, regardless of elementIsNullable/scalar" do
      property \(elementIsNullable, primitive) ->
        let v5Value =
              V5.Value.Value
                { dimensionality = 0,
                  elementIsNullable,
                  scalar = V5.Scalar.PrimitiveScalar primitive
                }
         in (V5.Value.toV4Value v5Value).arraySettings === Nothing

    it "property: dimensionality > 0 always maps to Just, preserving both fields exactly" do
      property \(NonZero dimensionality, elementIsNullable, primitive) ->
        let v5Value =
              V5.Value.Value
                { dimensionality,
                  elementIsNullable,
                  scalar = V5.Scalar.PrimitiveScalar primitive
                }
         in (V5.Value.toV4Value v5Value).arraySettings
              === Just V4.ArraySettings.ArraySettings {dimensionality, elementIsNullable}
