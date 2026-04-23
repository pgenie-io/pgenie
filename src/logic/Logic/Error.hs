module Logic.Error where

import Utils.Prelude

-- | Error report.
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
