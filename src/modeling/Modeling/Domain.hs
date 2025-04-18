module Modeling.Domain where

import Base.Prelude hiding (Enum)

data Query = Query
  { params :: Vector Param,
    result :: Vector ResultColumn
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
