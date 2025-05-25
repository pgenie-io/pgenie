{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module Logic.Algebra where

import Base.Prelude

-- | Application error.
data Error

data ProjectFileLoaded

data TemporaryDbCreated

data QueriesLoaded

type QueriesIntrospected = [QueryIntrospected]

data QueriesMetadataLoaded

type QueriesListed = [QueryListed]

data QueryListed

data QuerySqlLoaded = QuerySqlLoaded
  { sql :: Text
  }

data QuerySqlParsed = QuerySqlParsed
  {
  }

data QueryIntrospected

data ArgsLoaded

data CodeGenerated

data QuerySignatureLoaded

data QueriesMetadataMerged

type MigrationsListed = [MigrationListed]

data MigrationListed

data MigrationLoaded

data MigrationExecuted

class (MonadError Error m) => Algebra m where
  runParallelly :: (forall f. (Applicative f) => (forall a. m a -> f a) -> f a) -> m a
  loadArgs :: m ArgsLoaded
  loadProjectFile :: ArgsLoaded -> m ProjectFileLoaded
  createTemporaryDb :: ProjectFileLoaded -> m TemporaryDbCreated
  dropTemporaryDb :: TemporaryDbCreated -> m ()
  listMigrations :: ProjectFileLoaded -> m MigrationsListed
  loadMigration :: MigrationListed -> m MigrationLoaded
  executeMigration :: TemporaryDbCreated -> MigrationLoaded -> m MigrationExecuted
  listQueries :: ProjectFileLoaded -> m QueriesListed
  loadQuerySql :: QueryListed -> m QuerySqlLoaded
  loadQuerySignature :: ProjectFileLoaded -> QueryListed -> m QuerySignatureLoaded
  parseQuerySql :: QuerySqlLoaded -> m QuerySqlParsed
  introspectQuery :: TemporaryDbCreated -> QuerySqlParsed -> m QueryIntrospected
  mergeQueryMetadata :: QueryIntrospected -> QuerySignatureLoaded -> m QueriesMetadataMerged
