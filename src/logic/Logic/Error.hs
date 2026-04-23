module Logic.Error where

import Utils.Prelude

-- | Error report.
data Error = Error
  { path :: [Text],
    message :: Text,
    suggestion :: Maybe Text,
    details :: [(Text, Text)]
  }
  deriving stock (Eq, Show)

nest :: [Text] -> Error -> Error
nest path err =
  let newPath = err.path <> path
   in err {path = newPath}

nesting :: (MonadError Error m) => [Text] -> m a -> m a
nesting path action =
  catchError action \err ->
    throwError (nest path err)
