module AppAlgebra.Migrations where

import Base.Prelude hiding (writeFile)
import ParallelismLogic qualified as Parallelism
import ReportingLogic.Algebra qualified as ReportingLogic

-- * Model

-- | Migrations error.
data Error

type MigrationsListed = [MigrationListed]

type MigrationsLoaded = [MigrationLoaded]

type MigrationsExecuted = [MigrationExecuted]

data MigrationListed

data MigrationLoaded

data MigrationExecuted

-- * Effect

class
  ( MonadError Error m,
    Parallelism.Parallelism m,
    ReportingLogic.Reports m
  ) =>
  ControlsMigrations m
  where
  listMigrations :: FilePath -> m MigrationsListed
  loadMigration :: MigrationListed -> m MigrationLoaded
  executeMigration :: MigrationLoaded -> m MigrationExecuted

-- * Logic

executeMigrationsAtPath ::
  (ControlsMigrations m) =>
  FilePath ->
  m MigrationsExecuted
executeMigrationsAtPath path = do
  migrationsListed <- listMigrations path
  migrationsLoaded <-
    Parallelism.runParallelly do
      for
        migrationsListed
        (Parallelism.parallelly . loadMigration)
  for migrationsLoaded executeMigration
