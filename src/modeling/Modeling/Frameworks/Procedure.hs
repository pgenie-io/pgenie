module Modeling.Frameworks.Procedure where

import Base.Prelude
import HasqlDev qualified as Hasql
import TextBuilder qualified

class Procedure params where
  type ProcedureResult params
  runProcedure ::
    ( Hasql.RunsSession m,
      MonadReader Location m,
      MonadError Error m,
      MonadWriter [Error] m
    ) =>
    params ->
    m (ProcedureResult params)

data Error = Error
  { location :: [Text],
    reason :: Text
  }
  deriving stock (Show, Eq)

type Location = [Text]

inContext :: (MonadReader Location m) => [TextBuilder] -> m a -> m a
inContext context = local (TextBuilder.toText (mconcat context) :)

warn :: (MonadReader Location m, MonadWriter [Error] m) => [TextBuilder] -> m ()
warn reason = do
  location <- ask
  tell [Error location (TextBuilder.toText (mconcat reason))]

crash :: (MonadReader Location m, MonadError Error m) => [TextBuilder] -> m a
crash reason = do
  location <- ask
  throwError (Error location (TextBuilder.toText (mconcat reason)))

runStatementByParams ::
  ( Hasql.RunsSession m,
    MonadReader Location m,
    MonadError Error m,
    MonadWriter [Error] m,
    Hasql.IsStatementParams params
  ) =>
  params ->
  m (Hasql.StatementResultByParams params)
runStatementByParams params =
  Hasql.runSession $ Hasql.runStatementByParams params
