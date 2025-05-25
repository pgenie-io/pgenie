-- | Command that generates code and missing signature files for the project.
--
-- Forces the intended use of the application. The user has no option not to generate the signature files.
module Logic.Commands.Generate where

import Base.Prelude
import Logic.Algebra

data Generate = Generate

instance ModelsCommand Generate where
  commandName _ =
    "generate"

  commandDescription _ =
    "Generate code and missing signature files for the project"

  commandArgParser =
    pure Generate

  commandProcedure Generate = do
    projectFileLoaded <- loadProjectFile
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
    queriesMetadataMerged <- runParallelly \parallelly ->
      for queriesListed \queryListed -> parallelly do
        (queryIntrospected, querySignatureLoaded) <- runParallelly \parallelly ->
          (,)
            <$> parallelly do
              querySqlLoaded <- loadQuerySql queryListed
              querySqlParsed <- parseQuerySql querySqlLoaded
              introspectQuery temporaryDbCreated querySqlParsed
            <*> parallelly do
              loadQuerySignature projectFileLoaded queryListed
        mergeQueryMetadata queryIntrospected querySignatureLoaded
    generateCode projectFileLoaded queriesMetadataMerged
    pure ()
