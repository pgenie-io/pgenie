module Analysis where

import Analysis.Algebras.Procedure qualified as Procedure
import Analysis.Domain
import Analysis.Procedures qualified as Procedures
import Base.Prelude hiding (Enum)
import Data.Vector qualified as Vector
import HasqlDev qualified
import SyntacticClass qualified as Syntactic

analyse ::
  (Monad m, HasqlDev.RunsSession m) =>
  Text ->
  m (Either Procedure.Error (Query, [Procedure.Error]))
analyse query =
  flip runReaderT []
    $ runExceptT
    $ runWriterT
    $ do
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
                Procedure.crash ["Column name missing. Specify one using the AS clause."]
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
