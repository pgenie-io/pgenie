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
  embedProcedure ::
    (contained -> (Event contained -> IO ()) -> IO (Either (Error contained) a)) ->
    (container -> (Event container -> IO ()) -> IO (Either (Error container) a))

class IsProcedure procedure where
  type ProcedureContext procedure
  type ProcedureResult procedure

  proceed ::
    ProcedureContext procedure ->
    (Event (ProcedureContext procedure) -> IO ()) ->
    procedure ->
    IO (Either (Error (ProcedureContext procedure)) (ProcedureResult procedure))

runSubprocedure ::
  (ContainsService subservice service, IsProcedure subprocedure, ProcedureContext subprocedure ~ subservice) =>
  service ->
  (Event service -> IO ()) ->
  subprocedure ->
  IO (Either (Error service) (ProcedureResult subprocedure))
runSubprocedure service notify subprocedure = do
  embedProcedure
    (\contained notifyContained -> proceed contained notifyContained subprocedure)
    service
    notify
