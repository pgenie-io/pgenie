module Logic.Features.Report where

import Utils.Prelude

-- | Problem report.
data Report = Report
  { path :: [Text],
    message :: Text,
    suggestion :: Maybe Text,
    details :: [(Text, Text)]
  }
  deriving stock (Eq, Show)

nest :: [Text] -> Report -> Report
nest path err =
  let newPath = err.path <> path
   in err {path = newPath}

nesting :: (MonadError Report m) => [Text] -> m a -> m a
nesting path action =
  catchError action \err ->
    throwError (nest path err)

-- | Emission of non-fatal problem reports.
class (Monad m) => Warns m where
  warn :: Report -> m ()
