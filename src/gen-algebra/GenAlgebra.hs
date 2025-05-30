module GenAlgebra where

import Base.Prelude hiding (Enum)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson

-- * Interfaces

data Gen = forall generatorConfig. Gen
  { -- | Name of the config section.
    configSectionKey :: Text,
    -- | Major version of the codegen.
    version :: Int,
    -- | Specification of the parser of a section of the config file, where the section is identified by key and version.
    generatorConfigParser :: Aeson.Value -> Aeson.Parser generatorConfig,
    defaultGeneratorConfig :: generatorConfig,
    generate :: generatorConfig -> Project -> Either Error Artifact
  }

-- * Domain

data Artifact = Artifact
  { files :: [(FilePath, Text)]
  }

data Error

data Project = Project
  { name :: Name,
    version :: NonEmpty Word,
    customTypes :: Map Name CustomType,
    queries :: Map Name Query
  }
  deriving stock (Show, Eq)

data Query = Query
  { params :: NonEmpty (Name, Type),
    result :: QueryResult,
    fragments :: [QueryFragment]
  }
  deriving stock (Show, Eq)

data QueryResult
  = MaybeRowQueryResult RowSig
  | SingleRowQueryResult RowSig
  | MultiRowQueryResult RowSig
  | NoQueryResult
  deriving stock (Show, Eq)

type RowSig = NonEmpty (Name, Type)

data QueryFragment
  = SqlQueryFragment Text
  | VarQueryFragment Name
  deriving stock (Show, Eq)

-- | Name of a variable, parameter, or field.
--
-- Non-empty list of words in lower case, where word is a latin letter followed by any number of latin letters and digits.
type Name = NonEmpty Text

data Type = Type
  { isNullable :: Bool,
    dimensional :: Dimensional
  }
  deriving stock (Show, Eq)

data Dimensional = Dimensional
  { dimensionality :: Int,
    scalar :: Scalar
  }
  deriving stock (Show, Eq)

data Scalar
  = PrimitiveScalar Primitive
  | -- | Custom type reference.
    -- Use the name to look up the definition in the array of custom types provided in 'Project'.
    CustomScalar Name
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

data CustomType
  = CompositeCustomType (Map Text Type)
  | EnumCustomType (Vector Text)
  | DomainCustomType Dimensional
  deriving stock (Show, Eq)
