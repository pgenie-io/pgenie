{-# LANGUAGE AllowAmbiguousTypes #-}

module GenContractV4.Contract.DhallSpec (spec) where

import Data.Either.Validation qualified as Validation
import Data.Set qualified as Set
import Dhall qualified
import Dhall.Core qualified
import Dhall.Map qualified
import GenContractV4.Contract qualified as Contract
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "Dhall/Haskell model compatibility" do
    it "Name record fields match" do
      checkRecordFields @Contract.Name
        (contract <> ".Name")

    it "Version record fields match" do
      checkRecordFields @Contract.Version
        (contract <> ".Version")

    it "ArraySettings record fields match" do
      checkRecordFields @Contract.ArraySettings
        (contract <> ".ArraySettings")

    it "Value record fields match" do
      checkRecordFields @Contract.Value
        (contract <> ".Value")

    it "Member record fields match" do
      checkRecordFields @Contract.Member
        (contract <> ".Member")

    it "EnumVariant record fields match" do
      checkRecordFields @Contract.EnumVariant
        (contract <> ".EnumVariant")

    it "CustomType record fields match" do
      checkRecordFields @Contract.CustomType
        (contract <> ".CustomType")

    it "ResultRows record fields match" do
      checkRecordFields @Contract.ResultRows
        (contract <> ".ResultRows")

    it "Var record fields match" do
      checkRecordFields @Contract.Var
        (contract <> ".Var")

    it "Query record fields match" do
      checkRecordFields @Contract.Query
        (contract <> ".Query")

    it "Project record fields match" do
      checkRecordFields @Contract.Project
        (contract <> ".Project")

    it "Primitive union alternatives match" do
      checkUnionAlternatives @Contract.Primitive
        (contract <> ".Primitive")

    it "Scalar union alternatives match" do
      checkUnionAlternatives @Contract.Scalar
        (contract <> ".Scalar")

    it "CustomTypeDefinition union alternatives match" do
      checkUnionAlternatives @Contract.CustomTypeDefinition
        (contract <> ".CustomTypeDefinition")

    it "ResultRowsCardinality union alternatives match" do
      checkUnionAlternatives @Contract.ResultRowsCardinality
        (contract <> ".ResultRowsCardinality")

    it "Result union alternatives match" do
      checkUnionAlternatives @Contract.Result
        (contract <> ".Result")

    it "QueryFragment union alternatives match" do
      checkUnionAlternatives @Contract.QueryFragment
        (contract <> ".QueryFragment")

    it "ResultRowsCardinality Optional serializes/deserializes" do
      checkSerialization
        Contract.OptionalResultRowsCardinality
        (contract <> ".ResultRowsCardinality.Optional")

    it "Result RowsAffected serializes/deserializes" do
      checkSerialization
        Contract.RowsAffectedResult
        (contract <> ".Result.RowsAffected")

    it "Scalar Primitive payload serializes/deserializes" do
      checkSerialization
        (Contract.PrimitiveScalar Contract.Int4Primitive)
        (contract <> ".Scalar.Primitive " <> contract <> ".Primitive.Int4")

    it "Name serializes/deserializes" do
      checkSerialization
        exampleName
        "{ inCamelCase = \"userId\", inPascalCase = \"UserId\", inKebabCase = \"user-id\", inTrainCase = \"User-Id\", inScreamingKebabCase = \"USER-ID\", inSnakeCase = \"user_id\", inCamelSnakeCase = \"User_Id\", inScreamingSnakeCase = \"USER_ID\" }"

exampleName :: Contract.Name
exampleName =
  Contract.Name
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

-- | Pinned reference to the pGenie generator contract, v4.
contract :: Text
contract = "(./test/Contract.dhall)"
