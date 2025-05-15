{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

-- | Adaptation of the `LibPQ` library to the `Service` framework.
module App.Services.PqConnection.Context
  ( Context (..),
    Config (..),
    Error (..),
    Event,
  )
where

import App.Frameworks.Service
import Base.Prelude
import Database.PostgreSQL.LibPQ qualified as Pq

newtype Context = Context Pq.Connection

data instance Config Context
  = Config
      -- | Connection string.
      Text

data instance Error Context
  = ConnectionError
  | ResultError
      -- | SQLSTATE code.
      Text
      -- | Message.
      Text
      -- | Offset in the associated query string.
      (Maybe Int)
  deriving stock (Show, Eq)

data instance Event Context

instance IsService Context where
  start (Config connectionString) _ _ = do
    connection <- Pq.connectdb (to connectionString)
    result <- Pq.status connection
    case result of
      Pq.ConnectionBad -> pure (NotStarted 0 0 "Can't connect")
      _ -> pure (Started (Context connection))
  stop (Context conn) = Pq.finish conn

instance IsSome Pq.Connection Context where
  to (Context conn) = conn

instance IsSome Context Pq.Connection where
  to = Context

instance IsMany Pq.Connection Context

instance IsMany Context Pq.Connection

instance Is Pq.Connection Context

instance Is Context Pq.Connection
