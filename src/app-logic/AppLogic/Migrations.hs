module AppLogic.Migrations where

import AlgebraicPath qualified as Path
import Base.Prelude
import ParallelismAlgebra
import StagingAlgebra.Algebra

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
    Parallelism m,
    Reports m
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
  stage "Executing migrations" 2 do
    migrationsListed <- listMigrations path

    let migrationsCount = length migrationsListed

    migrationsLoaded <-
      stage "Loading" migrationsCount do
        runParallelly do
          for migrationsListed \migrationListed ->
            parallelly do
              stage (Path.toText migrationListed.path) 0 do
                migrationLoaded <- loadMigration migrationListed
                pure (migrationListed.path, migrationLoaded)

    stage "Executing" migrationsCount do
      for migrationsLoaded \(path, migrationLoaded) -> do
        stage (Path.toText path) 0 do
          executeMigration migrationLoaded