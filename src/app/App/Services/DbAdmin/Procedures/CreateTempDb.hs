module App.Services.DbAdmin.Procedures.CreateTempDb
  ( CreateTempDb (..),
    CreateTempDbResult,
  )
where

import App.Algebras.Service
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
        Hasql.Session.script
          $ from @TextBuilder
          $ mconcat
          $ [ "CREATE USER ",
              to dbName,
              " WITH PASSWORD '",
              to dbName,
              "';\n",
              "CREATE DATABASE ",
              to dbName,
              " OWNER ",
              to dbName,
              ";"
            ]
        pure (TempDbHandle dbName)

dbName :: UUID -> Text
dbName uuid =
  "_" <> Text.replace "-" "_" (Uuid.toText uuid)
