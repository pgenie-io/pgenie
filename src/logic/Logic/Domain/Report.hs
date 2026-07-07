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

nest :: [Text] -> Report -> Report
nest path err =
  let newPath = err.path <> path
   in err {path = newPath}

nestingAsError :: (MonadError Report m) => [Text] -> m a -> m a
nestingAsError path action =
  catchError action \err ->
    throwError (nest path err)
