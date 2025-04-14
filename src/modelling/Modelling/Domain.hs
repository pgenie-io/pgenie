module Modelling.Domain where

import Base.Prelude hiding (Enum)

-- * Errors

data Error
  = ConnectionError
  | UnsupportedFeature
      -- | Feature.
      Text
      -- | Input.
      Text
  deriving stock (Show, Eq)

-- * Types

data Type
  = PrimitiveType Primitive
  | ArrayType Array
  | CompositeType Composite
  | EnumType Enum
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

data Array = Array
  { dimensions :: Int,
    element :: ArrayElement
  }
  deriving stock (Show, Eq)

data ArrayElement
  = PrimitiveArrayElement Primitive
  | CompositeArrayElement Composite
  | EnumArrayElement Enum
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
