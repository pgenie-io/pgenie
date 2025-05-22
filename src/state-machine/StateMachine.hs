module StateMachine where

import Base.Prelude

-- | Application error.
data Error

data ProjectFileLoaded

data MigrationsLoaded

data TemporaryDbCreated

data QueriesLoaded

type QueriesAnalyzed = [QueryAnalyzed]

data QueriesMetadataLoaded

type QueriesListed = [QueryListed]

data QueryListed

data QueryLoaded = QueryLoaded
  { sql :: Text
  }

data QueryParsed = QueryParsed
  {
  }

data QueryAnalyzed

class (MonadError Error m) => Algebra m where
  runParallelly :: (forall f. (Applicative f) => (m a -> f a) -> f a) -> m a
  loadProjectFile :: m ProjectFileLoaded
  loadMigrations :: ProjectFileLoaded -> m MigrationsLoaded
  executeMigrations :: MigrationsLoaded -> m ()
  createTemporaryDb :: ProjectFileLoaded -> m TemporaryDbCreated
  dropTemporaryDb :: TemporaryDbCreated -> m ()
  listQueries :: ProjectFileLoaded -> m QueriesListed
  loadQuery :: QueryListed -> m QueryLoaded
  parseQuery :: QueryLoaded -> m QueryParsed
  analyzeQuery :: QueryLoaded -> QueryParsed -> m QueryAnalyzed

analyze :: (Algebra m) => m QueriesAnalyzed
analyze = do
  projectFile <- loadProjectFile
  migrationsLoaded <- runParallelly \parallelly -> do
    migrationsLoaded <- parallelly do
      loadMigrations projectFile
    temporaryDbCreated <- parallelly do
      createTemporaryDb projectFile
    pure migrationsLoaded
  executeMigrations migrationsLoaded
  queriesListed <- listQueries projectFile
  runParallelly \parallelly ->
    forM queriesListed \queryListed -> parallelly do
      queryLoaded <- loadQuery queryListed
      queryParsed <- parseQuery queryLoaded
      analyzeQuery queryLoaded queryParsed
