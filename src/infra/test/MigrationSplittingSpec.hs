module MigrationSplittingSpec (spec) where

import Data.Text qualified as Text
import Fx
import Infra.Adapters.Analyser qualified as Analyser
import Logic.Capabilities.Migrations
import Logic.Domain.Report (Report (..))
import Test.Hspec
import Utils.Prelude

spec :: Spec
spec = describe "executeMigration" do
  it "regression #74: runs two CREATE INDEX CONCURRENTLY statements in one migration file" do
    -- Sending the whole file as a single Simple Query message puts it in an
    -- implicit transaction block, which CREATE INDEX CONCURRENTLY rejects
    -- with a 25001 error even though the file has no explicit BEGIN.
    result <- runWithAnalyser do
      executeMigration
        "create table a (id int4 not null primary key, x int4 not null);\n\
        \create table b (id int4 not null primary key, y int4 not null);"
      executeMigration
        "create index concurrently a_x_idx on a (x);\n\
        \create index concurrently b_y_idx on b (y);"
    case result of
      Left err -> expectationFailure ("Expected migration to succeed, but failed: " <> show err)
      Right () -> pure ()

  it "still honors an explicit BEGIN/COMMIT pair written in a migration file" do
    result <- runWithAnalyser do
      executeMigration
        "begin;\n\
        \create table t (id int4 not null primary key);\n\
        \commit;"
    case result of
      Left err -> expectationFailure ("Expected migration to succeed, but failed: " <> show err)
      Right () -> pure ()

  it "attributes a failure to the failing statement, not the whole file" do
    result <- runWithAnalyser do
      executeMigration
        "create table t (id int4 not null primary key);\n\
        \create table t (id int4 not null primary key);"
    case result of
      Left err -> err.path `shouldBe` ["line 2"]
      Right () -> expectationFailure "Expected the duplicate CREATE TABLE to fail"

  it "reports only the failing statement's SQL, not the entire migration file" do
    result <- runWithAnalyser do
      executeMigration
        "create table only_first (id int4 not null primary key);\n\
        \select 1 / 0;"
    case result of
      Left err -> case lookup "sql" err.details of
        Just sql -> sql `shouldNotSatisfy` Text.isInfixOf "only_first"
        Nothing -> expectationFailure "Expected a 'sql' detail on the error"
      Right () -> expectationFailure "Expected division by zero to fail"

runWithAnalyser ::
  Fx Analyser.Device Report a ->
  IO (Either Report a)
runWithAnalyser action =
  action
    & scoping (Analyser.scope (Analyser.DockerSource {postgresTag = "postgres:18", reuseContainer = False}) (const (pure ())))
    & exposeErr
    & Fx.runFx
