module AppAlgebra.Migrations where

import AlgebraicPath qualified as Path
import Base.Prelude
import ParallelismLogic qualified as Parallelism
import ReportingLogic.Algebra qualified as ReportingLogic

-- * Model

-- | Migrations error.
data Error

type MigrationsListed = [MigrationListed]

type MigrationsLoaded = [MigrationLoaded]

type MigrationsExecuted = [MigrationExecuted]

data MigrationListed = MigrationListed
  { path :: Path
  }

data MigrationLoaded

data MigrationExecuted

-- * Effect

class
  ( IsSome e Error,
    MonadError e m,
    Parallelism.Parallelism m,
    ReportingLogic.Reports m
  ) =>
  ControlsMigrations e m
  where
  listMigrations :: Path -> m MigrationsListed
  loadMigration :: MigrationListed -> m MigrationLoaded
  executeMigration :: MigrationLoaded -> m MigrationExecuted

-- * Logic

executeMigrationsAtPath ::
  (ControlsMigrations e m) =>
  Path ->
  m MigrationsExecuted
executeMigrationsAtPath path = do
  migrationsListed <- listMigrations path
  migrationsLoaded <-
    Parallelism.runParallelly do
      for
        migrationsListed
        (Parallelism.parallelly . loadMigration)
  for migrationsLoaded executeMigration
