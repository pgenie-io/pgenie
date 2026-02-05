module App.Frameworks.Service where

import Base.Prelude

data family Config service

data family Event service

data family Error service

type ServiceIO service a =
  service -> (Event service -> IO ()) -> IO (Either (Error service) a)

class IsService service where
  start ::
    Config service ->
    -- | Notify function.
    (Event service -> IO ()) ->
    -- | Attempt count.
    Int ->
    IO (Started service)
  stop :: service -> IO ()

data Started service
  = NotStarted
      -- | Attempts left. Terminal when 0.
      Int
      -- | Sleeping time.
      DiffTime
      -- | Reason.
      Text
  | Started service

class ContainsConfig sub super where
  extractConfig :: super -> sub

class (IsService contained, IsService container) => ContainsService contained container where
  embedProcedure :: ServiceIO contained a -> ServiceIO container a

class IsProcedure procedure where
  type ProcedureContext procedure
  type ProcedureResult procedure

  proceed ::
    procedure ->
    ServiceIO (ProcedureContext procedure) (ProcedureResult procedure)

runSubprocedure ::
  (ContainsService subservice service, IsProcedure subprocedure, ProcedureContext subprocedure ~ subservice) =>
  subprocedure ->
  ServiceIO service (ProcedureResult subprocedure)
runSubprocedure subprocedure = embedProcedure (proceed subprocedure)
