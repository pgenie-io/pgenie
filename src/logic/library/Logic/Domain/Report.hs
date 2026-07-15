-- |
-- A structured problem report used as the common error type across the
-- logic layer's Ports, carrying a path to nest reports from sub-operations
-- and an optional suggestion for the user.
module Logic.Domain.Report
  ( Report (..),
    nest,
    nestingAsError,
  )
where

import Utils.Prelude

-- | Problem report.
data Report = Report
  { path :: [Text],
    message :: Text,
    suggestion :: Maybe Text,
    details :: [(Text, Text)]
  }
  deriving stock (Eq, Show)

instance IsString Report where
  fromString message =
    Report
      { path = [],
        message = fromString message,
        suggestion = Nothing,
        details = []
      }

-- | Prepend a path segment to a report, e.g. to identify the sub-operation
-- that produced it as errors propagate up through callers.
nest :: [Text] -> Report -> Report
nest path err =
  let newPath = err.path <> path
   in err {path = newPath}

-- | Run an action, nesting the given path onto any report it throws.
nestingAsError :: (MonadError Report m) => [Text] -> m a -> m a
nestingAsError path action =
  catchError action \err ->
    throwError (nest path err)
