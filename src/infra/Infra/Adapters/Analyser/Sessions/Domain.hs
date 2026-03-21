module Infra.Adapters.Analyser.Sessions.Domain where

import Utils.Prelude hiding (Enum)

data Query = Query
  { params :: Vector Param,
    resultColumns :: Vector ResultColumn
  }
  deriving stock (Show, Eq)

data Param = Param
  { nullable :: Bool,
    type_ :: Type
  }
  deriving stock (Show, Eq)

data ResultColumn = ResultColumn
  { name :: Text,
    nullable :: Bool,
    type_ :: Type
  }
  deriving stock (Show, Eq)

data Type = Type
  { dimensionality :: Int,
    scalar :: Scalar
  }
  deriving stock (Show, Eq)

data Scalar
  = PrimitiveScalar Primitive
  | CompositeScalar Composite
  | EnumScalar Enum
  deriving stock (Show, Eq)

data Primitive
  = -- | bool — OID 16, array OID 1000
    BoolPrimitive
  | -- | bytea — OID 17, array OID 1001
    ByteaPrimitive
  | -- | "char" — OID 18, array OID 1002. Internal single-byte type, not char(n).
    CharPrimitive
  | -- | cidr — OID 650, array OID 651
    CidrPrimitive
  | -- | date — OID 1082, array OID 1182
    DatePrimitive
  | -- | datemultirange — OID 4535, array OID 6155
    DatemultirangePrimitive
  | -- | daterange — OID 3912, array OID 3913
    DaterangePrimitive
  | -- | float4 — OID 700, array OID 1021
    Float4Primitive
  | -- | float8 — OID 701, array OID 1022
    Float8Primitive
  | -- | inet — OID 869, array OID 1041
    InetPrimitive
  | -- | int2 — OID 21, array OID 1005
    Int2Primitive
  | -- | int4 — OID 23, array OID 1007
    Int4Primitive
  | -- | int4multirange — OID 4451, array OID 6150
    Int4multirangePrimitive
  | -- | int4range — OID 3904, array OID 3905
    Int4rangePrimitive
  | -- | int8 — OID 20, array OID 1016
    Int8Primitive
  | -- | int8multirange — OID 4536, array OID 6157
    Int8multirangePrimitive
  | -- | int8range — OID 3926, array OID 3927
    Int8rangePrimitive
  | -- | interval — OID 1186, array OID 1187
    IntervalPrimitive
  | -- | json — OID 114, array OID 199
    JsonPrimitive
  | -- | jsonb — OID 3802, array OID 3807
    JsonbPrimitive
  | -- | macaddr — OID 829, array OID 1040
    MacaddrPrimitive
  | -- | macaddr8 — OID 774, array OID 775
    Macaddr8Primitive
  | -- | money — OID 790, array OID 791
    MoneyPrimitive
  | -- | numeric — OID 1700, array OID 1231
    NumericPrimitive
  | -- | nummultirange — OID 4532, array OID 6151
    NummultirangePrimitive
  | -- | numrange — OID 3906, array OID 3907
    NumrangePrimitive
  | -- | text — OID 25, array OID 1009
    TextPrimitive
  | -- | time — OID 1083, array OID 1183
    TimePrimitive
  | -- | timestamp — OID 1114, array OID 1115
    TimestampPrimitive
  | -- | timestamptz — OID 1184, array OID 1185
    TimestamptzPrimitive
  | -- | timetz — OID 1266, array OID 1270
    TimetzPrimitive
  | -- | tsmultirange — OID 4533, array OID 6152
    TsmultirangePrimitive
  | -- | tsrange — OID 3908, array OID 3909
    TsrangePrimitive
  | -- | tstzmultirange — OID 4534, array OID 6153
    TstzmultirangePrimitive
  | -- | tstzrange — OID 3910, array OID 3911
    TstzrangePrimitive
  | -- | uuid — OID 2950, array OID 2951
    UuidPrimitive
  | -- | xml — OID 142, array OID 143
    XmlPrimitive
  | -- | varchar — OID 1043, array OID 1015
    VarcharPrimitive
  | -- | bpchar (char(n)) — OID 1042, array OID 1014
    BpcharPrimitive
  | -- | bit — OID 1560, array OID 1561
    BitPrimitive
  | -- | varbit — OID 1562, array OID 1563
    VarbitPrimitive
  | -- | tsvector — OID 3614, array OID 3643
    TsvectorPrimitive
  | -- | tsquery — OID 3615, array OID 3645
    TsqueryPrimitive
  | -- | point — OID 600, array OID 1017
    PointPrimitive
  | -- | line — OID 628, array OID 629
    LinePrimitive
  | -- | lseg — OID 601, array OID 1018
    LsegPrimitive
  | -- | box — OID 603, array OID 1020
    BoxPrimitive
  | -- | path — OID 602, array OID 1019
    PathPrimitive
  | -- | polygon — OID 604, array OID 1027
    PolygonPrimitive
  | -- | circle — OID 718, array OID 719
    CirclePrimitive
  | -- | pg_snapshot — OID 5038, array OID 5039
    PgSnapshotPrimitive
  | -- | pg_lsn — OID 3220, array OID 3221
    PgLsnPrimitive
  | -- | name — OID 19, array OID 1003
    NamePrimitive
  | -- | hstore — extension type, no static OIDs
    HstorePrimitive
  deriving stock (Show, Eq)

data Composite = Composite
  { schemaName :: Text,
    name :: Text,
    fields :: Vector CompositeField
  }
  deriving stock (Show, Eq)

data CompositeField = CompositeField
  { name :: Text,
    type_ :: Type
  }
  deriving stock (Show, Eq)

data Enum = Enum
  { schemaName :: Text,
    name :: Text,
    options :: Vector Text
  }
  deriving stock (Show, Eq)
