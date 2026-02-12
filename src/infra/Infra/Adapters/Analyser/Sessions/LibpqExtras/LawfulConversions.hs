{-# OPTIONS_GHC -Wno-orphans #-}

module Infra.Adapters.Analyser.Sessions.LibpqExtras.LawfulConversions where

import Base.Prelude
import Database.PostgreSQL.LibPQ qualified as Pq

-- * Oid

instance IsSome Word32 Pq.Oid where
  to (Pq.Oid base) = fromIntegral base
  maybeFrom = Just . to

instance IsMany Word32 Pq.Oid where
  onfrom = to

instance Is Word32 Pq.Oid

instance IsSome Pq.Oid Word32 where
  to = Pq.Oid . fromIntegral
  maybeFrom = Just . to

instance IsMany Pq.Oid Word32 where
  onfrom = to

instance Is Pq.Oid Word32

-- * Column

instance IsSome Int32 Pq.Column where
  to (Pq.Col base) = fromIntegral base
  maybeFrom = Just . to

instance IsMany Int32 Pq.Column where
  onfrom = to

instance Is Int32 Pq.Column

instance IsSome Pq.Column Int32 where
  to = Pq.Col . fromIntegral
  maybeFrom = Just . to

instance IsMany Pq.Column Int32 where
  onfrom = to

instance Is Pq.Column Int32
