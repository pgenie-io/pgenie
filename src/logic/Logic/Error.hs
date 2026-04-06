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
  let newPath = path <> err.path
   in err {path = newPath}
