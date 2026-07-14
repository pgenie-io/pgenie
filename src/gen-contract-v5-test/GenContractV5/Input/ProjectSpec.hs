module GenContractV5.Input.ProjectSpec (spec) where

import GenContractV4.Input.Member qualified as V4.Member
import GenContractV4.Input.Name qualified as V5.Name
import GenContractV4.Input.Project qualified as V4.Project
import GenContractV4.Input.Query qualified as V4.Query
import GenContractV4.Input.Result qualified as V4.Result
import GenContractV4.Input.ResultRows qualified as V4.ResultRows
import GenContractV4.Input.Scalar qualified as V4.Scalar
import GenContractV4.Input.Value qualified as V4.Value
import GenContractV5.Fixtures.Project1 qualified as Fixtures.Project1
import GenContractV5.Input.Project qualified as V5.Project
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "toV4Project on the Project1 fixture" do
    it "downgrades the status column's CustomScalar reference down to a bare Name, dropping pgSchema/pgName/index" do
      let v4Project = V5.Project.toV4Project Fixtures.Project1.input
      case v4Project of
        V4.Project.Project {queries = [v4Query]} ->
          case v4Query of
            V4.Query.Query {result = V4.Result.RowsResult V4.ResultRows.ResultRows {columns}} ->
              case filter (\member -> member.pgName == "status") (toList columns) of
                [V4.Member.Member {value = V4.Value.Value {scalar}}] ->
                  scalar `shouldBe` V4.Scalar.CustomScalar exampleStatusName
                other -> expectationFailure ("Expected exactly one status column, got: " <> show (length other))
            _ -> expectationFailure "Expected a RowsResult"
        V4.Project.Project {queries} ->
          expectationFailure ("Expected exactly one query, got: " <> show (length queries))

exampleStatusName :: V5.Name.Name
exampleStatusName =
  V5.Name.Name
    { inCamelCase = "status",
      inPascalCase = "Status",
      inKebabCase = "status",
      inTrainCase = "Status",
      inScreamingKebabCase = "STATUS",
      inSnakeCase = "status",
      inCamelSnakeCase = "Status",
      inScreamingSnakeCase = "STATUS"
    }
