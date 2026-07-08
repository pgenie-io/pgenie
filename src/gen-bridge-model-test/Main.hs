{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Data.Either.Validation qualified as Validation
import Data.Set qualified as Set
import Data.Text (Text)
import Dhall qualified
import Dhall.Core qualified
import Dhall.Map qualified
import PGenieGen.Model.Input qualified as Input
import Test.Hspec
import Prelude

-- | Pinned reference to the pGenie generator contract, at a specific commit.
-- The sha256 pins the fully-resolved Dhall expression as an integrity check.
--
-- https://github.com/pgenie-io/gen-contract
contract :: Text
contract =
  "(https://raw.githubusercontent.com/pgenie-io/gen-contract/2e5a24caee29099b3aff89b036f11a2b630c7371/src/package.dhall\
  \ sha256:7ee818a7147e18f180a80fcd2b2dcc11a3bb7efe1859d49fad2dccc37430fcec)"

main :: IO ()
main = hspec do
  describe "Dhall/Haskell model compatibility" do
    it "Version record fields match" $
      checkRecordFields @Input.Version
        (contract <> ".Version")

    it "ArraySettings record fields match" $
      checkRecordFields @Input.ArraySettings
        (contract <> ".ArraySettings")

    it "Value record fields match" $
      checkRecordFields @Input.Value
        (contract <> ".Value")

    it "Member record fields match" $
      checkRecordFields @Input.Member
        (contract <> ".Member")

    it "EnumVariant record fields match" $
      checkRecordFields @Input.EnumVariant
        (contract <> ".EnumVariant")

    it "CustomType record fields match" $
      checkRecordFields @Input.CustomType
        (contract <> ".CustomType")

    it "ResultRows record fields match" $
      checkRecordFields @Input.ResultRows
        (contract <> ".ResultRows")

    it "Var record fields match" $
      checkRecordFields @Input.Var
        (contract <> ".Var")

    it "Query record fields match" $
      checkRecordFields @Input.Query
        (contract <> ".Query")

    it "Project record fields match" $
      checkRecordFields @Input.Project
        (contract <> ".Project")

    it "Primitive union alternatives match" $
      checkUnionAlternatives @Input.Primitive
        (contract <> ".Primitive")

    it "Scalar union alternatives match" $
      checkUnionAlternatives @Input.Scalar
        (contract <> ".Scalar")

    it "CustomTypeDefinition union alternatives match" $
      checkUnionAlternatives @Input.CustomTypeDefinition
        (contract <> ".CustomTypeDefinition")

    it "ResultRowsCardinality union alternatives match" $
      checkUnionAlternatives @Input.ResultRowsCardinality
        (contract <> ".ResultRowsCardinality")

    it "Result union alternatives match" $
      checkUnionAlternatives @Input.Result
        (contract <> ".Result")

    it "QueryFragment union alternatives match" $
      checkUnionAlternatives @Input.QueryFragment
        (contract <> ".QueryFragment")

    it "ResultRowsCardinality Optional serializes/deserializes" $
      checkSerialization
        Input.OptionalResultRowsCardinality
        (contract <> ".ResultRowsCardinality.Optional")

    it "Result RowsAffected serializes/deserializes" $
      checkSerialization
        Input.RowsAffectedResult
        (contract <> ".Result.RowsAffected")

    it "Scalar Primitive payload serializes/deserializes" $
      checkSerialization
        (Input.PrimitiveScalar Input.Int4Primitive)
        (contract <> ".Scalar.Primitive " <> contract <> ".Primitive.Int4")

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
