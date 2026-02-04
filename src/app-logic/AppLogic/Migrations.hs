module AppLogic.Migrations where

import AlgebraicPath qualified as Path
import Base.Prelude
import ParallelismAlgebra
import StagingAlgebra.Algebra

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
              stage (Path.toText migrationListed) 0 do
                migrationLoaded <- loadMigration migrationListed
                pure (migrationListed, migrationLoaded)

    stage "Executing" migrationsCount do
      for migrationsLoaded \(migrationListed, migrationLoaded) -> do
        stage (Path.toText migrationListed) 0 do
          executeMigration migrationLoaded

-- * Model

-- | Migrations error.
data Error

type MigrationsLoaded = [MigrationLoaded]

type MigrationsExecuted = [MigrationExecuted]

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
  listMigrations :: Path -> m [Path]
  loadMigration :: Path -> m MigrationLoaded
  executeMigration :: MigrationLoaded -> m MigrationExecuted
