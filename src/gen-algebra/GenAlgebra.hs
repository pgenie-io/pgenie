{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module GenAlgebra where

import Base.Prelude hiding (Enum)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict qualified as Map

-- * Interfaces

-- | Interface for code generator adapters.
data Gen = forall generatorConfig. Gen
  { -- | Name of the config section.
    configSectionKey :: Text,
    -- | Major version of the codegen.
    version :: Int,
    -- | Specification of the parser of a section of the config file, where the section is identified by key and version.
    generatorConfigParser :: Aeson.Value -> Aeson.Parser generatorConfig,
    -- | Generate code for a project.
    generate :: generatorConfig -> Project -> Either Error [(FilePath, Text)]
  }

generate :: [Gen] -> Project -> Either Error [(FilePath, Text)]
generate adapters project = do
  error "TODO"
  where
    gensAvailMap :: Map (Text, Int) Gen
    gensAvailMap =
      adapters
        & fmap
          (\gen -> ((gen.configSectionKey, gen.version), gen))
        & Map.fromList

-- * Domain

data Error = Error

-- | Model of a project with all the data needed to generate code.
data Project = Project
  { name :: Name,
    version :: NonEmpty Int,
    customTypes :: Map Name CustomType,
    queries :: Map Name Query,
    generatorConfigs :: Map (Text, Int) Aeson.Value
  }
  deriving stock (Show, Eq)

data Query = Query
  { params :: NonEmpty (Name, Type),
    result :: QueryResult,
    fragments :: [QueryFragment]
  }
  deriving stock (Show, Eq)

data QueryResult
  = OptionalRowQueryResult (NonEmpty (Name, Type))
  | SingleRowQueryResult (NonEmpty (Name, Type))
  | MultiRowQueryResult (NonEmpty (Name, Type))
  | NoQueryResult
  deriving stock (Show, Eq)

data QueryFragment
  = SqlQueryFragment Text
  | VarQueryFragment Name
  deriving stock (Show, Eq)

-- | Name of a variable, parameter, or field.
--
-- Non-empty list of words in lower case, where word is a latin letter followed by any number of latin letters and digits.
type Name = NonEmpty Text

-- | Type signature.
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

data CustomType = CustomType
  { pgName :: Text,
    definition :: CustomTypeDefinition
  }
  deriving stock (Show, Eq)

data CustomTypeDefinition
  = CompositeCustomTypeDefinition (Map Name CompositeField)
  | EnumCustomTypeDefinition (Map Name EnumVariant)
  | DomainCustomTypeDefinition Dimensional
  deriving stock (Show, Eq)

data CompositeField = CompositeField
  { -- | Name of the field as it appears in the database.
    pgName :: Text,
    type_ :: Type
  }
  deriving stock (Show, Eq)

data EnumVariant = EnumVariant
  { pgName :: Text,
    index :: Int
  }
  deriving stock (Show, Eq)
