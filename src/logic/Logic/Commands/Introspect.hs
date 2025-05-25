module Logic.Commands.Introspect where

import Base.Prelude
import Logic.Algebra

runIntrospectApp :: (Algebra m) => m [QueriesMetadataMerged]
runIntrospectApp = do
  args <- loadArgs
  projectFileLoaded <- loadProjectFile args
  (temporaryDbCreated, queriesListed) <- runParallelly \parallelly -> do
    temporaryDbCreated <- parallelly do
      (temporaryDbCreated, migrationsListed) <- runParallelly \parallelly ->
        (,)
          <$> parallelly (createTemporaryDb projectFileLoaded)
          <*> parallelly (listMigrations projectFileLoaded)
      forM_ migrationsListed \migrationListed -> do
        migrationLoaded <- loadMigration migrationListed
        executeMigration temporaryDbCreated migrationLoaded
      pure temporaryDbCreated
    queriesListed <- parallelly (listQueries projectFileLoaded)
    pure (temporaryDbCreated, queriesListed)
  runParallelly \parallelly -> for queriesListed \queryListed -> parallelly do
    (queryIntrospected, querySignatureLoaded) <- runParallelly \parallelly ->
      (,)
        <$> parallelly do
          querySqlLoaded <- loadQuerySql queryListed
          querySqlParsed <- parseQuerySql querySqlLoaded
          introspectQuery temporaryDbCreated querySqlParsed
        <*> parallelly do
          loadQuerySignature projectFileLoaded queryListed
    mergeQueryMetadata queryIntrospected querySignatureLoaded
