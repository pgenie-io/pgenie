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
executeMigrationsAtPath path =
  ReportingLogic.stage "Executing migrations" 2 do
    migrationsListed <- listMigrations path

    let migrationsCount = length migrationsListed

    migrationsLoaded <-
      ReportingLogic.stage "Loading" migrationsCount do
        Parallelism.runParallelly do
          for migrationsListed \migrationListed ->
            Parallelism.parallelly do
              ReportingLogic.stage (Path.toText migrationListed.path) 1 do
                migrationLoaded <- loadMigration migrationListed
                pure (migrationListed.path, migrationLoaded)

    ReportingLogic.stage "Executing" migrationsCount do
      for migrationsLoaded \(path, migrationLoaded) -> do
        ReportingLogic.stage (Path.toText path) 1 do
          executeMigration migrationLoaded
