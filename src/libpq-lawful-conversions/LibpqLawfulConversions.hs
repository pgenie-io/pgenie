{-# OPTIONS_GHC -Wno-orphans #-}

module LibpqLawfulConversions where

import Data.Int
import Data.Word
import Database.PostgreSQL.LibPQ qualified as Pq
import LawfulConversions
import Prelude

-- * Oid

instance IsSome Word32 Pq.Oid where
  to (Pq.Oid base) = fromIntegral base
  maybeFrom = Just . to

instance IsMany Word32 Pq.Oid where
  from = to

instance Is Word32 Pq.Oid

instance IsSome Pq.Oid Word32 where
  to = Pq.Oid . fromIntegral
  maybeFrom = Just . to

instance IsMany Pq.Oid Word32 where
  from = to

instance Is Pq.Oid Word32

-- * Column

instance IsSome Int32 Pq.Column where
  to (Pq.Col base) = fromIntegral base
  maybeFrom = Just . to

instance IsMany Int32 Pq.Column where
  from = to

instance Is Int32 Pq.Column

instance IsSome Pq.Column Int32 where
  to = Pq.Col . fromIntegral
  maybeFrom = Just . to

instance IsMany Pq.Column Int32 where
  from = to

instance Is Pq.Column Int32
