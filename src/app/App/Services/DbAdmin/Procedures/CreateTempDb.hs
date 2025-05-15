module App.Services.DbAdmin.Procedures.CreateTempDb
  ( CreateTempDb (..),
    CreateTempDbResult,
  )
where

import App.Frameworks.Service
import App.Services.DbAdmin.Context
import App.Services.DbAdmin.Domain
import Base.Prelude
import Data.Text qualified as Text
import Data.UUID qualified as Uuid
import Data.UUID.V4 qualified as Uuid
import Hasql.Pool qualified
import Hasql.Session qualified

data CreateTempDb = CreateTempDb

type CreateTempDbResult = TempDbHandle

instance IsProcedure CreateTempDb where
  type ProcedureContext CreateTempDb = Context
  type ProcedureResult CreateTempDb = CreateTempDbResult

  proceed _ context _ = do
    dbName <- dbName <$> Uuid.nextRandom
    fmap (first Error) do
      Hasql.Pool.use context.pool do
        Hasql.Session.sql
          $ to
          $ mconcat
          $ [ "CREATE USER ",
              dbName,
              " WITH PASSWORD '",
              dbName,
              "';\n",
              "CREATE DATABASE ",
              dbName,
              " OWNER ",
              dbName,
              ";"
            ]
        pure (TempDbHandle dbName)

dbName :: UUID -> Text
dbName uuid =
  "_" <> Text.replace "-" "_" (Uuid.toText uuid)
