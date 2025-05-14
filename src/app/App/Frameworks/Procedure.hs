module App.Frameworks.Procedure where

import App.Frameworks.Service

class IsProcedure procedure where
  type ProcedureContext procedure
  type ProcedureResult procedure

  proceed ::
    procedure ->
    ServiceIO (ProcedureContext procedure) (ProcedureResult procedure)
