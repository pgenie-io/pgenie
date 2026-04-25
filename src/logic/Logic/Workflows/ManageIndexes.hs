module Logic.Workflows.ManageIndexes where

import AlgebraicPath qualified as Path
import Data.Text qualified as Text
import Logic.Features.Fs (FsOps (..))
import Logic.Features.IndexOptimizer (DropReason (..), IndexAction (..), IndexInfo (..))
import Logic.Features.IndexOptimizer qualified as IndexOptimizer
import Logic.Features.ProjectFile qualified as ProjectFile
import Logic.Features.Report (Report (..), Warns (..))
import Logic.Features.SeqScanDetector (SeqScanFinding (..))
import Logic.Features.Staging (Stages (..))
import Logic.Workflows.AnalyseProject qualified as AnalyseProject
import Utils.Prelude hiding (readFile, writeFile)

type Port m = (AnalyseProject.Port m, FsOps m, Warns m)

data Params = Params
  { allowRedundantIndexes :: Bool,
    -- | When set, write the generated migration to a numbered file in the
    -- @migrations/@ directory in addition to printing it to stdout.
    addMigration :: Bool
  }
  deriving stock (Eq, Show)

type Result = Text

run :: (Port m) => ProjectFile.ProjectFile -> Params -> m Result
run projectFile params =
  stage "" 2 do
    analyseResult <- AnalyseProject.run projectFile
    let queryNeeds =
          map (\(_, finding) -> (finding.tableName, finding.suggestedIndexColumns)) analyseResult.seqScanFindings
        allActions = IndexOptimizer.optimizeIndexes analyseResult.indexes queryNeeds
        (dropActions, createActions) =
          partition
            ( \case
                DropIndex {} -> True
                CreateIndex {} -> False
            )
            allActions
        migrationActions =
          if params.allowRedundantIndexes
            then createActions
            else allActions
    when params.allowRedundantIndexes do
      for_ dropActions \action ->
        warn (Report [] (indexActionMessage action) (Just "Suppressed by --allow-redundant-indexes") (indexActionDetails action))
    if null migrationActions
      then pure ""
      else do
        let migrationContent = IndexOptimizer.generateMigration migrationActions
        when params.addMigration do
          writeMigrationFile migrationContent
        pure migrationContent
  where
    indexActionMessage :: IndexAction -> Text
    indexActionMessage (DropIndex idx reason) = case reason of
      ExactDuplicate other ->
        "Redundant index: "
          <> idx.indexName
          <> " on "
          <> idx.tableName
          <> " is an exact duplicate of "
          <> other.indexName
      PrefixRedundancy other ->
        "Redundant index: "
          <> idx.indexName
          <> " on "
          <> idx.tableName
          <> " is a prefix of "
          <> other.indexName
      ExcessiveComposite replacement ->
        "Excessive composite index: "
          <> idx.indexName
          <> " on "
          <> idx.tableName
          <> " can be narrowed to ("
          <> Text.intercalate ", " replacement
          <> ")"
      UnusedByQueries ->
        "Redundant index: "
          <> idx.indexName
          <> " on "
          <> idx.tableName
          <> " is not used by observed query needs"
    indexActionMessage (CreateIndex tbl cols) =
      "Missing index: "
        <> tbl
        <> " needs an index on ("
        <> Text.intercalate ", " cols
        <> ")"

    indexActionDetails :: IndexAction -> [(Text, Text)]
    indexActionDetails (DropIndex idx _) = [("index", idx.indexName), ("table", idx.tableName)]
    indexActionDetails (CreateIndex tbl cols) = [("table", tbl), ("columns", Text.intercalate ", " cols)]

    writeMigrationFile :: (Port m) => Text -> m ()
    writeMigrationFile content = do
      migrationsListed <-
        listDir "migrations"
          & fmap (filter (\p -> Path.toExtensions p == ["sql"]))
          & fmap sort
      for_ migrationsListed \p -> do
        let baseName = Path.toBasename p
        unless (not (Text.null baseName) && Text.all isDigit baseName) do
          throwError
            ( Report
                []
                ("Migration naming convention is not clear: " <> Path.toText p)
                (Just "All migration files must follow the N.sql naming convention (e.g., 1.sql, 2.sql)")
                [("file", Path.toText p)]
            )
      let nextMigrationNum = length migrationsListed + 1
          migrationFileName = Text.pack (show nextMigrationNum) <> ".sql"
      case Path.maybeFromText ("migrations/" <> migrationFileName) of
        Nothing ->
          throwError
            ( Report
                []
                "Failed to construct migration file path"
                Nothing
                [("filename", migrationFileName)]
            )
        Just migrationPath -> do
          writeFile migrationPath content
          warn
            ( Report
                []
                ("Written migration to migrations/" <> migrationFileName)
                (Just "Review the migration and commit it")
                []
            )
