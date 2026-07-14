module GenContractV5.Input.ScalarSpec (spec) where

import GenContractV4.Input.Name qualified as V5.Name
import GenContractV4.Input.Primitive qualified as V5.Primitive
import GenContractV4.Input.Scalar qualified as V4.Scalar
import GenContractV5.Input.CustomTypeRef qualified as V5.CustomTypeRef
import GenContractV5.Input.Scalar qualified as V5.Scalar
import Test.Hspec

spec :: Spec
spec = do
  describe "toV4Scalar" do
    it "keeps a primitive scalar unchanged" do
      V5.Scalar.toV4Scalar (V5.Scalar.PrimitiveScalar V5.Primitive.Int4Primitive)
        `shouldBe` V4.Scalar.PrimitiveScalar V5.Primitive.Int4Primitive

    it "drops pgSchema/pgName/index, keeping only name" do
      V5.Scalar.toV4Scalar
        ( V5.Scalar.CustomScalar
            V5.CustomTypeRef.CustomTypeRef
              { name = exampleName,
                pgSchema = "public",
                pgName = "status",
                index = 3
              }
        )
        `shouldBe` V4.Scalar.CustomScalar exampleName

exampleName :: V5.Name.Name
exampleName =
  V5.Name.Name
    { inCamelCase = "userId",
      inPascalCase = "UserId",
      inKebabCase = "user-id",
      inTrainCase = "User-Id",
      inScreamingKebabCase = "USER-ID",
      inSnakeCase = "user_id",
      inCamelSnakeCase = "User_Id",
      inScreamingSnakeCase = "USER_ID"
    }
