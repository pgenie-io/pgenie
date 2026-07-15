module GenContractV5.Input.ValueSpec (spec) where

import GenContractV4.Input qualified as V4
import GenContractV5.Input qualified as V5
import GenContractV5.Input.Value qualified as V5.Value
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "toV4Value" do
    it "maps dimensionality = 0 to arraySettings = Nothing" do
      V5.Value.toV4Value
        V5.Value
          { dimensionality = 0,
            elementIsNullable = True,
            scalar = V5.PrimitiveScalar V5.Int4Primitive
          }
        `shouldBe` V4.Value
          { arraySettings = Nothing,
            scalar = V4.PrimitiveScalar V4.Int4Primitive
          }

    it "maps dimensionality > 0 to Just an ArraySettings carrying the same two fields" do
      V5.Value.toV4Value
        V5.Value
          { dimensionality = 2,
            elementIsNullable = True,
            scalar = V5.PrimitiveScalar V5.TextPrimitive
          }
        `shouldBe` V4.Value
          { arraySettings = Just V4.ArraySettings {dimensionality = 2, elementIsNullable = True},
            scalar = V4.PrimitiveScalar V4.TextPrimitive
          }

    it "property: dimensionality = 0 always maps to Nothing, regardless of elementIsNullable/scalar" do
      property \(elementIsNullable, primitive) ->
        let v5Value =
              V5.Value
                { dimensionality = 0,
                  elementIsNullable,
                  scalar = V5.PrimitiveScalar primitive
                }
         in (V5.Value.toV4Value v5Value).arraySettings === Nothing

    it "property: dimensionality > 0 always maps to Just, preserving both fields exactly" do
      property \(NonZero dimensionality, elementIsNullable, primitive) ->
        let v5Value =
              V5.Value
                { dimensionality,
                  elementIsNullable,
                  scalar = V5.PrimitiveScalar primitive
                }
         in (V5.Value.toV4Value v5Value).arraySettings
              === Just V4.ArraySettings {dimensionality, elementIsNullable}
