module App.Frameworks.Service where

import Base.Prelude

data family Config service

data family Event service

data family Error service

class IsService service where
  start ::
    Config service ->
    -- | Attempt count.
    Int ->
    IO (Started service)
  stop :: service -> IO ()

data Started service
  = NotStarted
      -- | Attempts left. Terminal when 0.
      Int
      -- | Sleeping time.
      DiffTime
      -- | Reason.
      Text
  | Started service

type ServiceIO service a =
  service -> (Event service -> IO ()) -> IO (Either (Error service) a)

class (IsService contained, IsService container) => ContainsService contained container where
  projectConfig :: Config container -> Config contained
  onContained ::
    (contained -> (Event contained -> IO ()) -> IO (Either (Error contained) a)) ->
    (container -> (Event container -> IO ()) -> IO (Either (Error container) a))
