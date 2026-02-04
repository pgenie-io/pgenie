module Analysis.Domain where

import Base.Prelude hiding (Enum)

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
  = BoolPrimitive
  | ByteaPrimitive
  | CharPrimitive
  | CidrPrimitive
  | DatePrimitive
  | DatemultirangePrimitive
  | DaterangePrimitive
  | Float4Primitive
  | Float8Primitive
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
  | MacaddrPrimitive
  | Macaddr8Primitive
  | MoneyPrimitive
  | NumericPrimitive
  | NummultirangePrimitive
  | NumrangePrimitive
  | TextPrimitive
  | TimePrimitive
  | TimestampPrimitive
  | TimestamptzPrimitive
  | TimetzPrimitive
  | TsmultirangePrimitive
  | TsrangePrimitive
  | TstzmultirangePrimitive
  | TstzrangePrimitive
  | UuidPrimitive
  | XmlPrimitive
  deriving stock (Show, Eq)

data Composite = Composite
  { name :: Text,
    fields :: Vector CompositeField
  }
  deriving stock (Show, Eq)

data CompositeField = CompositeField
  { name :: Text,
    type_ :: Type
  }
  deriving stock (Show, Eq)

data Enum = Enum
  { name :: Text,
    options :: Vector Text
  }
  deriving stock (Show, Eq)

-- | Table of static associations between OIDs and types.
typeOidTable :: [(Int32, Type)]
typeOidTable =
  [ (17, Type 0 (PrimitiveScalar ByteaPrimitive)),
    (18, Type 0 (PrimitiveScalar CharPrimitive)),
    (20, Type 0 (PrimitiveScalar Int8Primitive)),
    (21, Type 0 (PrimitiveScalar Int2Primitive)),
    (23, Type 0 (PrimitiveScalar Int4Primitive)),
    (25, Type 0 (PrimitiveScalar TextPrimitive)),
    (114, Type 0 (PrimitiveScalar JsonPrimitive)),
    (142, Type 0 (PrimitiveScalar XmlPrimitive)),
    (700, Type 0 (PrimitiveScalar Float4Primitive)),
    (701, Type 0 (PrimitiveScalar Float8Primitive)),
    (790, Type 0 (PrimitiveScalar MoneyPrimitive)),
    (829, Type 0 (PrimitiveScalar MacaddrPrimitive)),
    (869, Type 0 (PrimitiveScalar InetPrimitive)),
    (650, Type 0 (PrimitiveScalar CidrPrimitive)),
    (774, Type 0 (PrimitiveScalar Macaddr8Primitive)),
    (1082, Type 0 (PrimitiveScalar DatePrimitive)),
    (1083, Type 0 (PrimitiveScalar TimePrimitive)),
    (1114, Type 0 (PrimitiveScalar TimestampPrimitive)),
    (1184, Type 0 (PrimitiveScalar TimestamptzPrimitive)),
    (1186, Type 0 (PrimitiveScalar IntervalPrimitive)),
    (1266, Type 0 (PrimitiveScalar TimetzPrimitive)),
    (1700, Type 0 (PrimitiveScalar NumericPrimitive)),
    (2950, Type 0 (PrimitiveScalar UuidPrimitive)),
    (3802, Type 0 (PrimitiveScalar JsonbPrimitive)),
    (16, Type 0 (PrimitiveScalar BoolPrimitive)),
    (3904, Type 0 (PrimitiveScalar Int4rangePrimitive)),
    (3910, Type 0 (PrimitiveScalar TstzrangePrimitive)),
    (3906, Type 0 (PrimitiveScalar NumrangePrimitive)),
    (3926, Type 0 (PrimitiveScalar Int8rangePrimitive)),
    (3908, Type 0 (PrimitiveScalar TsrangePrimitive)),
    (3912, Type 0 (PrimitiveScalar DaterangePrimitive)),
    (4451, Type 0 (PrimitiveScalar Int4multirangePrimitive)),
    (4532, Type 0 (PrimitiveScalar NummultirangePrimitive)),
    (4533, Type 0 (PrimitiveScalar TsmultirangePrimitive)),
    (4534, Type 0 (PrimitiveScalar TstzmultirangePrimitive)),
    (4535, Type 0 (PrimitiveScalar DatemultirangePrimitive)),
    (4536, Type 0 (PrimitiveScalar Int8multirangePrimitive)),
    (1001, Type 1 (PrimitiveScalar ByteaPrimitive)),
    (1002, Type 1 (PrimitiveScalar CharPrimitive)),
    (1016, Type 1 (PrimitiveScalar Int8Primitive)),
    (1005, Type 1 (PrimitiveScalar Int2Primitive)),
    (1007, Type 1 (PrimitiveScalar Int4Primitive)),
    (1009, Type 1 (PrimitiveScalar TextPrimitive)),
    (199, Type 1 (PrimitiveScalar JsonPrimitive)),
    (143, Type 1 (PrimitiveScalar XmlPrimitive)),
    (1021, Type 1 (PrimitiveScalar Float4Primitive)),
    (1022, Type 1 (PrimitiveScalar Float8Primitive)),
    (791, Type 1 (PrimitiveScalar MoneyPrimitive)),
    (1040, Type 1 (PrimitiveScalar MacaddrPrimitive)),
    (1041, Type 1 (PrimitiveScalar InetPrimitive)),
    (651, Type 1 (PrimitiveScalar CidrPrimitive)),
    (775, Type 1 (PrimitiveScalar Macaddr8Primitive)),
    (1182, Type 1 (PrimitiveScalar DatePrimitive)),
    (1183, Type 1 (PrimitiveScalar TimePrimitive)),
    (1115, Type 1 (PrimitiveScalar TimestampPrimitive)),
    (1185, Type 1 (PrimitiveScalar TimestamptzPrimitive)),
    (1187, Type 1 (PrimitiveScalar IntervalPrimitive)),
    (1270, Type 1 (PrimitiveScalar TimetzPrimitive)),
    (1231, Type 1 (PrimitiveScalar NumericPrimitive)),
    (2951, Type 1 (PrimitiveScalar UuidPrimitive)),
    (3807, Type 1 (PrimitiveScalar JsonbPrimitive)),
    (1000, Type 1 (PrimitiveScalar BoolPrimitive)),
    (3905, Type 1 (PrimitiveScalar Int4rangePrimitive)),
    (3911, Type 1 (PrimitiveScalar TstzrangePrimitive)),
    (3907, Type 1 (PrimitiveScalar NumrangePrimitive)),
    (3927, Type 1 (PrimitiveScalar Int8rangePrimitive)),
    (3909, Type 1 (PrimitiveScalar TsrangePrimitive)),
    (3913, Type 1 (PrimitiveScalar DaterangePrimitive)),
    (6150, Type 1 (PrimitiveScalar Int4multirangePrimitive)),
    (6151, Type 1 (PrimitiveScalar NummultirangePrimitive)),
    (6152, Type 1 (PrimitiveScalar TsmultirangePrimitive)),
    (6153, Type 1 (PrimitiveScalar TstzmultirangePrimitive)),
    (6155, Type 1 (PrimitiveScalar DatemultirangePrimitive)),
    (6157, Type 1 (PrimitiveScalar Int8multirangePrimitive))
  ]
