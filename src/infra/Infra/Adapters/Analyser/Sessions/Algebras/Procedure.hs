module Infra.Adapters.Analyser.Sessions.Algebras.Procedure where

import Base.Prelude
import Hasql.Mapping.IsStatement qualified as Hasql
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
    reason :: Text,
    details :: [(Text, Text)]
  }
  deriving stock (Show, Eq)

type Location = [Text]

inContext :: (MonadReader Location m) => [TextBuilder] -> m a -> m a
inContext context = local (TextBuilder.toText (mconcat context) :)

warn :: (MonadReader Location m, MonadWriter [Error] m) => [TextBuilder] -> m ()
warn reason = do
  location <- ask
  tell [Error location (TextBuilder.toText (mconcat reason)) []]

crash :: (MonadReader Location m, MonadError Error m) => [TextBuilder] -> [(Text, Text)] -> m a
crash reason details = do
  location <- ask
  throwError (Error location (TextBuilder.toText (mconcat reason)) details)

runStatementByParams ::
  ( Hasql.RunsSession m,
    Hasql.IsStatement params
  ) =>
  params ->
  m (Hasql.Result params)
runStatementByParams params =
  Hasql.runSession $ Hasql.runStatementByParams params
