{-# LANGUAGE AllowAmbiguousTypes #-}

module GenBridge.Model.InputDhallSpec (spec) where

import Data.Either.Validation qualified as Validation
import Data.Set qualified as Set
import Dhall qualified
import Dhall.Core qualified
import Dhall.Map qualified
import GenBridge.Model.Input qualified as Input
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "Dhall/Haskell model compatibility" do
    it "Name record fields match" do
      checkRecordFields @Input.Name
        (contract <> ".Name")

    it "Version record fields match" do
      checkRecordFields @Input.Version
        (contract <> ".Version")

    it "ArraySettings record fields match" do
      checkRecordFields @Input.ArraySettings
        (contract <> ".ArraySettings")

    it "Value record fields match" do
      checkRecordFields @Input.Value
        (contract <> ".Value")

    it "Member record fields match" do
      checkRecordFields @Input.Member
        (contract <> ".Member")

    it "EnumVariant record fields match" do
      checkRecordFields @Input.EnumVariant
        (contract <> ".EnumVariant")

    it "CustomType record fields match" do
      checkRecordFields @Input.CustomType
        (contract <> ".CustomType")

    it "CustomTypeRef record fields match" do
      checkRecordFields @Input.CustomTypeRef
        (contract <> ".CustomTypeRef")

    it "ResultRows record fields match" do
      checkRecordFields @Input.ResultRows
        (contract <> ".ResultRows")

    it "Var record fields match" do
      checkRecordFields @Input.Var
        (contract <> ".Var")

    it "Query record fields match" do
      checkRecordFields @Input.Query
        (contract <> ".Query")

    it "Project record fields match" do
      checkRecordFields @Input.Project
        (contract <> ".Project")

    it "Primitive union alternatives match" do
      checkUnionAlternatives @Input.Primitive
        (contract <> ".Primitive")

    it "Scalar union alternatives match" do
      checkUnionAlternatives @Input.Scalar
        (contract <> ".Scalar")

    it "CustomTypeDefinition union alternatives match" do
      checkUnionAlternatives @Input.CustomTypeDefinition
        (contract <> ".CustomTypeDefinition")

    it "ResultRowsCardinality union alternatives match" do
      checkUnionAlternatives @Input.ResultRowsCardinality
        (contract <> ".ResultRowsCardinality")

    it "Result union alternatives match" do
      checkUnionAlternatives @Input.Result
        (contract <> ".Result")

    it "QueryFragment union alternatives match" do
      checkUnionAlternatives @Input.QueryFragment
        (contract <> ".QueryFragment")

    it "ResultRowsCardinality Optional serializes/deserializes" do
      checkSerialization
        Input.OptionalResultRowsCardinality
        (contract <> ".ResultRowsCardinality.Optional")

    it "Result RowsAffected serializes/deserializes" do
      checkSerialization
        Input.RowsAffectedResult
        (contract <> ".Result.RowsAffected")

    it "Scalar Primitive payload serializes/deserializes" do
      checkSerialization
        (Input.PrimitiveScalar Input.Int4Primitive)
        (contract <> ".Scalar.Primitive " <> contract <> ".Primitive.Int4")

    it "Name serializes/deserializes" do
      checkSerialization
        exampleName
        "{ inCamelCase = \"userId\", inPascalCase = \"UserId\", inKebabCase = \"user-id\", inTrainCase = \"User-Id\", inScreamingKebabCase = \"USER-ID\", inSnakeCase = \"user_id\", inCamelSnakeCase = \"User_Id\", inScreamingSnakeCase = \"USER_ID\" }"

    it "CustomTypeRef serializes/deserializes" do
      checkSerialization
        (Input.CustomTypeRef
          { name = exampleName,
            pgSchema = "public",
            pgName = "user_id",
            index = 0
          }
        )
        ( "{ name = "
            <> "{ inCamelCase = \"userId\", inPascalCase = \"UserId\", inKebabCase = \"user-id\", inTrainCase = \"User-Id\", inScreamingKebabCase = \"USER-ID\", inSnakeCase = \"user_id\", inCamelSnakeCase = \"User_Id\", inScreamingSnakeCase = \"USER_ID\" }"
            <> ", pgSchema = \"public\""
            <> ", pgName = \"user_id\""
            <> ", index = 0"
            <> " }"
        )

    it "CustomScalar serializes/deserializes" do
      checkSerialization
        (Input.CustomScalar
          Input.CustomTypeRef
            { name = exampleName,
              pgSchema = "public",
              pgName = "user_id",
              index = 0
            }
        )
        ( contract <> ".Scalar.Custom "
            <> "{ name = "
            <> "{ inCamelCase = \"userId\", inPascalCase = \"UserId\", inKebabCase = \"user-id\", inTrainCase = \"User-Id\", inScreamingKebabCase = \"USER-ID\", inSnakeCase = \"user_id\", inCamelSnakeCase = \"User_Id\", inScreamingSnakeCase = \"USER_ID\" }"
            <> ", pgSchema = \"public\""
            <> ", pgName = \"user_id\""
            <> ", index = 0"
            <> " }"
        )

exampleName :: Input.Name
exampleName =
  Input.Name
    { inCamelCase = "userId",
      inPascalCase = "UserId",
      inKebabCase = "user-id",
      inTrainCase = "User-Id",
      inScreamingKebabCase = "USER-ID",
      inSnakeCase = "user_id",
      inCamelSnakeCase = "User_Id",
      inScreamingSnakeCase = "USER_ID"
    }

-- | Assert that a Dhall record type expression and a Haskell 'Decoder'
-- describe the same set of record fields.
checkRecordFields :: forall a. (Dhall.FromDhall a) => Text -> IO ()
checkRecordFields dhallExpr = do
  actual <- Dhall.inputExpr dhallExpr
  case Dhall.expected (Dhall.auto @a) of
    Validation.Failure errs ->
      expectationFailure ("Haskell Decoder has invalid expected type: " ++ show errs)
    Validation.Success expr -> do
      let dhallFields = recordFields actual
      let hsFields = recordFields expr
      dhallFields `shouldBe` hsFields

-- | Assert that a Dhall union type expression and a Haskell 'Decoder' describe
-- the same set of union alternatives.  For non-union types the comparison falls
-- back to checking that both sides produce an empty set, which will pass
-- trivially; add dedicated record-field tests if broader coverage is needed.
checkUnionAlternatives :: forall a. (Dhall.FromDhall a) => Text -> IO ()
checkUnionAlternatives dhallExpr = do
  actual <- Dhall.inputExpr dhallExpr
  case Dhall.expected (Dhall.auto @a) of
    Validation.Failure errs ->
      expectationFailure ("Haskell Decoder has invalid expected type: " ++ show errs)
    Validation.Success expr -> do
      let dhallAlts = unionAlternatives actual
      let hsAlts = unionAlternatives expr
      dhallAlts `shouldBe` hsAlts

-- | Extract the set of alternative names from a Dhall union type expression.
unionAlternatives :: Dhall.Core.Expr s a -> Set.Set Text
unionAlternatives = \case
  Dhall.Core.Union alts -> Set.fromList (Dhall.Map.keys alts)
  _ -> Set.empty

-- | Extract the set of field names from a Dhall record type expression.
recordFields :: Dhall.Core.Expr s a -> Set.Set Text
recordFields = \case
  Dhall.Core.Record fields -> Set.fromList (Dhall.Map.keys fields)
  _ -> Set.empty

-- | Assert a Haskell value round-trips through a specific Dhall expression
-- and that encoding uses an expression judgmentally equal to that expression.
checkSerialization :: forall a. (Eq a, Show a, Dhall.FromDhall a, Dhall.ToDhall a) => a -> Text -> IO ()
checkSerialization value dhallExpr = do
  decoded <- Dhall.input Dhall.auto dhallExpr
  decoded `shouldBe` value

  expected <- Dhall.inputExpr dhallExpr
  let encoded = Dhall.embed Dhall.inject value
  Dhall.Core.judgmentallyEqual encoded expected `shouldBe` True

-- | Pinned reference to the pGenie generator contract.
contract :: Text
contract = "(./src/gen-bridge-test/Contract.dhall)"
