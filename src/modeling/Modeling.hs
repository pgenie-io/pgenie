module Modeling where

import Base.Prelude hiding (Enum)
import Data.Vector qualified as Vector
import HasqlDev qualified
import Modeling.Domain
import Modeling.Frameworks.Procedure qualified as Procedure
import Modeling.Procedures qualified as Procedures
import SyntacticClass qualified as Syntactic

analyse ::
  ( HasqlDev.RunsSession m,
    MonadError Procedure.Error m,
    MonadWriter [Procedure.Error] m,
    MonadReader Procedure.Location m
  ) =>
  Text ->
  m Query
analyse query = do
  queryDescription <-
    Procedure.runProcedure Procedures.DescribeQuery {query}

  paramTypes <-
    Vector.iforM queryDescription.paramTypeOids \index paramTypeOid -> do
      Procedure.inContext ["param:", Syntactic.toTextBuilder (show index)] do
        Procedure.inContext ["oid:", Syntactic.toTextBuilder paramTypeOid] do
          Procedure.runProcedure
            Procedures.ResolveTypeByOid {oid = fromIntegral paramTypeOid}

  params <-
    Procedure.runProcedure Procedures.ResolveParamNullabilities {query, paramTypes}

  result <-
    error "TODO"

  pure Query {params, result}
