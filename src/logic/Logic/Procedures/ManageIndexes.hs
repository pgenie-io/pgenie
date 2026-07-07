module Logic.Procedures.ManageIndexes where

import AlgebraicPath qualified as Path
import Data.Text qualified as Text
import Logic.Capabilities.Fs (FsOps (..))
import Logic.Capabilities.Reporting (Warns (..))
import Logic.Capabilities.Staging (Stages (..))
import Logic.Domain.IndexOptimization (DropReason (..), IndexAction (..), IndexInfo (..))
import Logic.Domain.IndexOptimization qualified as IndexOptimizer
import Logic.Domain.ProjectFile qualified as ProjectFile
import Logic.Domain.Report (Report (..))
import Logic.Domain.SeqScanFinding (SeqScanFinding (..))
import Logic.Procedures.AnalyseProject qualified as AnalyseProject
import Utils.Prelude hiding (readFile, writeFile)

type Port m = (AnalyseProject.Port m, FsOps m, Warns m)

data Params = Params
  { projectFile :: ProjectFile.ProjectFile,
    allowRedundantIndexes :: Bool,
    -- | When set, write the generated migration to a numbered file in the
    -- @migrations/@ directory in addition to printing it to stdout.
    addMigration :: Bool
  }

data Result = Result
  { migrationText :: Text
  }

run :: (Port m) => Params -> m Result
run params =
  stage "" 2 do
    analyseResult <-
      AnalyseProject.run AnalyseProject.Params {projectFile = params.projectFile}
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
      then pure Result {migrationText = ""}
      else do
        let migrationContent = IndexOptimizer.generateMigration migrationActions
        when params.addMigration do
          writeMigrationFile migrationContent
        pure Result {migrationText = migrationContent}
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
      let existingNumbers = map (read . Text.unpack . Path.toBasename) migrationsListed :: [Int]
          nextMigrationNum = case existingNumbers of
            [] -> 1
            nums -> maximum nums + 1
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
          when (migrationPath `elem` migrationsListed) do
            throwError
              ( Report
                  []
                  ("Migration file already exists: migrations/" <> migrationFileName)
                  (Just "This is likely a bug; please report it")
                  [("file", migrationFileName)]
              )
          writeFile migrationPath content
          warn
            ( Report
                []
                ("Written migration to migrations/" <> migrationFileName)
                (Just "Review the migration and commit it")
                []
            )
