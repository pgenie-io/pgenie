{-# LANGUAGE TemplateHaskell #-}

-- |
-- PostgreSQL primitive scalar types supported by the contract model.
module GenContractV4.Contract.Primitive
  ( Primitive (..),
  )
where

import Dhall qualified
import GenContractBase.Aeson.Deriver qualified as Aeson.Deriver
import GenContractBase.Dhall.Deriving qualified as Dhall.Deriving
import GenContractBase.Dhall.Orphans ()
import Test.QuickCheck qualified as QuickCheck
import Utils.Prelude

-- | PostgreSQL primitive types.
--
-- Keep 'CharPrimitive' for the internal single-byte '"char"' type (OID 18),
-- which is distinct from 'char(n)' / 'bpchar'.
data Primitive
  = BitPrimitive
  | BoolPrimitive
  | BoxPrimitive
  | BpcharPrimitive
  | ByteaPrimitive
  | CharPrimitive
  | CidrPrimitive
  | CirclePrimitive
  | CitextPrimitive
  | DatePrimitive
  | DatemultirangePrimitive
  | DaterangePrimitive
  | Float4Primitive
  | Float8Primitive
  | HstorePrimitive
  | InetPrimitive
  | Int2Primitive
  | Int4Primitive
  | Int4multirangePrimitive
  | Int4rangePrimitive
  | Int8Primitive
  | Int8multirangePrimitive
  | Int8rangePrimitive
  | IntervalPrimitive
  | JsonPrimitive
  | JsonbPrimitive
  | LinePrimitive
  | LsegPrimitive
  | MacaddrPrimitive
  | Macaddr8Primitive
  | MoneyPrimitive
  | NamePrimitive
  | NumericPrimitive
  | NummultirangePrimitive
  | NumrangePrimitive
  | OidPrimitive
  | PathPrimitive
  | PgLsnPrimitive
  | PgSnapshotPrimitive
  | PointPrimitive
  | PolygonPrimitive
  | TextPrimitive
  | TimePrimitive
  | TimestampPrimitive
  | TimestamptzPrimitive
  | TimetzPrimitive
  | TsmultirangePrimitive
  | TsqueryPrimitive
  | TsrangePrimitive
  | TstzmultirangePrimitive
  | TstzrangePrimitive
  | TsvectorPrimitive
  | UuidPrimitive
  | VarbitPrimitive
  | VarcharPrimitive
  | XmlPrimitive
  | Box2DPrimitive
  | Box3DPrimitive
  | LtreePrimitive
  | GeometryPrimitive
  | GeographyPrimitive
  deriving stock (Show, Eq, Generic, Bounded, Enum)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Primitive") Primitive)

instance Arbitrary Primitive where
  arbitrary = QuickCheck.elements [minBound .. maxBound]

Aeson.Deriver.derive [''Primitive]
