module Modeling.Frameworks.Procedure where

import Base.Prelude
import TextBuilder qualified

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
