module GenContractV5Spec (spec) where

import GenContractV4.Contract qualified as V4
import GenContractV5 qualified as V5
import GenContractV5.Fixtures qualified as Fixtures
import GenContractVersioning
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "downgradeInput on the Project1 fixture" do
    it "downgrades the status column's CustomScalar reference down to a bare Name, dropping pgSchema/pgName/index" do
      case downgradeInput @V5.V5 Fixtures.input1 of
        Right v4Project ->
          case v4Project of
            V4.Project {queries = [v4Query]} ->
              case v4Query of
                V4.Query {result = V4.RowsResult V4.ResultRows {columns}} ->
                  case filter (\member -> member.pgName == "status") (toList columns) of
                    [V4.Member {value = V4.Value {scalar}}] ->
                      scalar `shouldBe` V4.CustomScalar exampleStatusName
                    other -> expectationFailure ("Expected exactly one status column, got: " <> show (length other))
                _ -> expectationFailure "Expected a RowsResult"
            V4.Project {queries} ->
              expectationFailure ("Expected exactly one query, got: " <> show (length queries))
        Left err -> expectationFailure ("Expected a successful downgrade, got: " <> toList err)

exampleStatusName :: V4.Name
exampleStatusName =
  V4.Name
    { inCamelCase = "status",
      inPascalCase = "Status",
      inKebabCase = "status",
      inTrainCase = "Status",
      inScreamingKebabCase = "STATUS",
      inSnakeCase = "status",
      inCamelSnakeCase = "Status",
      inScreamingSnakeCase = "STATUS"
    }
