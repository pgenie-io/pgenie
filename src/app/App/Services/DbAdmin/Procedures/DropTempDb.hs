module App.Services.DbAdmin.Procedures.DropTempDb
  ( DropTempDb (..),
    DropTempDbResult,
  )
where

import App.Frameworks.Service
import App.Services.DbAdmin.Context
import App.Services.DbAdmin.Domain
import Base.Prelude
import Hasql.Pool qualified
import Hasql.Session qualified

data DropTempDb = DropTempDb
  { handle :: TempDbHandle
  }
  deriving stock (Show, Eq)

type DropTempDbResult = ()

instance IsProcedure DropTempDb where
  type ProcedureContext DropTempDb = Context
  type ProcedureResult DropTempDb = DropTempDbResult

  proceed params context _ =
    fmap (first Error) do
      Hasql.Pool.use context.pool do
        let name = params.handle.userAndDbName
        Hasql.Session.sql
          $ to
          $ mconcat
          $ [ "DROP DATABASE ",
              name,
              ";\n",
              "DROP USER ",
              name,
              ";"
            ]
