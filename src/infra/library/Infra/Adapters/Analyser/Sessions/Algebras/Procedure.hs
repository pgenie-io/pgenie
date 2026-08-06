-- |
-- The aggregator-namespace contract implemented by every module under
-- @Infra.Adapters.Analyser.Sessions.Procedures@. Each procedure resolves one
-- piece of query-analysis information (a type, a column, a nullability) by
-- running Hasql statements while accumulating warnings and location context.
module Infra.Adapters.Analyser.Sessions.Algebras.Procedure
  ( IsProcedure (..),
    Error (..),
    Location,
    inContext,
    warn,
    crash,
    runStatementByParams,
  )
where

import Hasql.Mapping.IsStatement qualified
import HasqlDev qualified
import TextBuilder qualified
import Utils.Prelude

-- | Lift session-running through the transformer stack that 'runProcedure'
-- is instantiated at (see 'Infra.Adapters.Analyser.Sessions.inferTypes').
-- Upstream @hasql-dev@ used to provide these generically; it now only
-- defines the base 'HasqlDev.Session' instance.
instance (HasqlDev.RunsSession m) => HasqlDev.RunsSession (ReaderT r m) where
  runSession session = ReaderT \_ -> HasqlDev.runSession session

instance (HasqlDev.RunsSession m) => HasqlDev.RunsSession (ExceptT e m) where
  runSession session = ExceptT (fmap Right (HasqlDev.runSession session))

instance (HasqlDev.RunsSession m, Monoid w) => HasqlDev.RunsSession (WriterT w m) where
  runSession session = WriterT (fmap (,mempty) (HasqlDev.runSession session))

-- |
-- A unit of query-analysis work, parameterised by its input.
--
-- Implementors live one-per-module under @Sessions.Procedures@, each naming
-- its parameter type after the module itself (the Aggregator Namespace
-- pattern).
class IsProcedure params where
  -- | The information a procedure resolves, given its parameters.
  type ProcedureResult params

  -- | Run the procedure, in a context that can track location, fail with a
  -- crash, and accumulate non-fatal warnings.
  runProcedure ::
    ( HasqlDev.RunsSession m,
      MonadReader Location m,
      MonadError Error m,
      MonadWriter [Error] m
    ) =>
    params ->
    m (ProcedureResult params)

-- | A procedure failure or warning, carrying the location it occurred at.
data Error = Error
  { location :: [Text],
    reason :: Text,
    details :: [(Text, Text)]
  }
  deriving stock (Show, Eq)

-- | Breadcrumb trail identifying where in the query an error or warning occurred.
type Location = [Text]

-- | Push a breadcrumb onto the location trail for the duration of an action.
inContext :: (MonadReader Location m) => [TextBuilder] -> m a -> m a
inContext context = local (TextBuilder.toText (mconcat context) :)

-- | Record a non-fatal issue at the current location without aborting.
warn :: (MonadReader Location m, MonadWriter [Error] m) => [TextBuilder] -> m ()
warn reason = do
  location <- ask
  tell [Error location (TextBuilder.toText (mconcat reason)) []]

-- | Abort the current procedure with an error at the current location.
crash :: (MonadReader Location m, MonadError Error m) => [TextBuilder] -> [(Text, Text)] -> m a
crash reason details = do
  location <- ask
  throwError (Error location (TextBuilder.toText (mconcat reason)) details)

-- | Run a Hasql statement implicitly determined by its parameters.
runStatementByParams ::
  ( HasqlDev.RunsSession m,
    Hasql.Mapping.IsStatement.IsStatement params
  ) =>
  params ->
  m (Hasql.Mapping.IsStatement.Result params)
runStatementByParams params =
  HasqlDev.runSession $ HasqlDev.runStatementByParams params
