-- |
-- Runs the full chain of 'Procedure's needed to resolve a query's parameter
-- and result-column types, as a single Hasql session.
module Infra.Adapters.Analyser.Sessions
  ( Procedure.Error (..),

    -- * Sessions
    inferTypes,

    -- * Domain
    module Infra.Adapters.Analyser.Sessions.Domain,
  )
where

import Data.Vector qualified as Vector
import HasqlDev qualified
import Infra.Adapters.Analyser.Sessions.Algebras.Procedure qualified as Procedure
import Infra.Adapters.Analyser.Sessions.Domain
import Infra.Adapters.Analyser.Sessions.Procedures qualified as Procedures
import SyntacticClass qualified as Syntactic
import Utils.Prelude hiding (Enum)

-- |
-- The carrier for 'Procedure.IsProcedure': a concrete monad that carries
-- (implements) the effects 'Procedure.runProcedure' is written against --
-- tracking 'Procedure.Location' via 'MonadReader', failing via 'MonadError',
-- accumulating warnings via 'MonadWriter', and running Hasql sessions via
-- 'HasqlDev.RunsSession' -- by wrapping the one transformer stack those
-- effects are ever actually run at in 'inferTypes'.
--
-- Existing purely to supply that last instance, 'HasqlDev.RunsSession': the
-- other three effects already have generic @mtl@ instances for
-- 'ReaderT'\/'ExceptT'\/'WriterT', but @hasql-dev@ no longer provides the
-- analogous lifting instances for 'HasqlDev.RunsSession' (it now only
-- defines the base 'HasqlDev.Session' instance). Rather than reintroducing
-- those as orphan instances on 'ReaderT'\/'ExceptT'\/'WriterT' -- types this
-- module doesn't own -- 'Procede' packages the stack as a type of its own
-- and defines 'HasqlDev.RunsSession' on that directly.
newtype Procede a
  = Procede (WriterT [Procedure.Error] (ExceptT Procedure.Error (ReaderT Procedure.Location HasqlDev.Session)) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Procedure.Location,
      MonadError Procedure.Error,
      MonadWriter [Procedure.Error]
    )

instance HasqlDev.RunsSession Procede where
  runSession = Procede . lift . lift . lift . HasqlDev.runSession

-- | Unwrap a 'Procede' to run its underlying transformer stack.
runProcede :: Procede a -> WriterT [Procedure.Error] (ExceptT Procedure.Error (ReaderT Procedure.Location HasqlDev.Session)) a
runProcede (Procede m) = m

-- |
-- Describe the query via libpq, then resolve each parameter and result
-- column's Postgres type, returning either the first fatal error encountered
-- or the resolved 'Query' together with any non-fatal warnings.
inferTypes ::
  Text ->
  HasqlDev.Session (Either Procedure.Error (Query, [Procedure.Error]))
inferTypes query =
  flip runReaderT [] $ runExceptT $ runWriterT $ runProcede do
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

    resultColumns <-
      Vector.iforM queryDescription.resultColumns \index resultColumn -> do
        Procedure.inContext ["column:", Syntactic.toTextBuilder (show index)] do
          name <- case resultColumn.name of
            Nothing -> do
              Procedure.crash ["Column name missing. Specify one using the AS clause."] []
            Just name -> pure name
          Procedure.inContext ["name:", Syntactic.toTextBuilder name] do
            type_ <-
              Procedure.runProcedure
                Procedures.ResolveTypeByOid
                  { oid = fromIntegral resultColumn.typeOid
                  }
            Procedure.runProcedure
              Procedures.ResolveColumn
                { relationOid = fromIntegral resultColumn.tableOid,
                  attributeNum = resultColumn.tableColumnIndex
                }
              >>= \case
                Nothing -> do
                  pure (ResultColumn name True type_)
                Just tableColumn -> do
                  when (tableColumn.typeOid /= fromIntegral resultColumn.typeOid) do
                    Procedure.warn
                      [ "Column type OID mismatch. From query description: ",
                        Syntactic.toTextBuilder resultColumn.typeOid,
                        ", from referred table column: ",
                        Syntactic.toTextBuilder tableColumn.typeOid,
                        ". Using the first one."
                      ]
                  pure
                    ResultColumn
                      { name,
                        nullable = not tableColumn.notNull,
                        type_
                      }

    pure Query {params, resultColumns}
