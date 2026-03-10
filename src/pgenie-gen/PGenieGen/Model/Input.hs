module PGenieGen.Model.Input
  ( module PGenieGen.Model.Input,
    Name (..),
    Word (..),
    WordChar (..),
    WordOrNumber (..),
  )
where

import AesonDeriver qualified
import Dhall qualified
import Dhall.TH (GenerateOptions (..), HaskellType (..), defaultGenerateOptions, makeHaskellTypes, makeHaskellTypesWith)
import PGenieGen.Dhall.Deriving qualified as Dhall.Deriving
import PGenieGen.Dhall.Orphans ()
import PGenieGen.Model.Input.Name (Name (..))
import PGenieGen.Model.Input.Word (Word (..))
import PGenieGen.Model.Input.WordChar (WordChar (..))
import PGenieGen.Model.Input.WordOrNumber (WordOrNumber (..))
import PGenieGen.Prelude hiding (Version, Word)

-- | Version with semantic versioning components.
-- Generated from @src/pgenie-gen/dhall/Version.dhall@.
makeHaskellTypes
  [ SingleConstructor "Version" "Version" "./src/pgenie-gen/dhall/Version.dhall"
  , SingleConstructor "ArraySettings" "ArraySettings" "./src/pgenie-gen/dhall/ArraySettings.dhall"
  ]

deriving instance Show Version

deriving instance Eq Version

deriving instance Show ArraySettings

deriving instance Eq ArraySettings

-- | PostgreSQL primitive types.
-- Generated from @src/pgenie-gen/dhall/Primitive.dhall@.
makeHaskellTypesWith
  defaultGenerateOptions {constructorModifier = ("Primitive" <>)}
  [ MultipleConstructors "Primitive" "./src/pgenie-gen/dhall/Primitive.dhall"
  ]

deriving instance Show Primitive

deriving instance Eq Primitive

-- | Category of result rows.
-- Generated from @src/pgenie-gen/dhall/ResultRowsCardinality.dhall@.
makeHaskellTypesWith
  defaultGenerateOptions {constructorModifier = ("ResultRowsCardinality" <>)}
  [ MultipleConstructors "ResultRowsCardinality" "./src/pgenie-gen/dhall/ResultRowsCardinality.dhall"
  ]

deriving instance Show ResultRowsCardinality

deriving instance Eq ResultRowsCardinality

-- | Either a primitive type or a custom type
data Scalar
  = ScalarPrimitive Primitive
  | ScalarCustom Name
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Scalar") Scalar)

-- | A value with optional array settings and scalar type
data Value = Value
  { arraySettings :: Maybe ArraySettings,
    scalar :: Scalar
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | A field in a composite type or query parameter
data Member = Member
  { name :: Name,
    pgName :: Text,
    isNullable :: Bool,
    value :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | A variant in an enum type
data EnumVariant = EnumVariant
  { name :: Name,
    pgName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | Definition of a custom type
data CustomTypeDefinition
  = CustomTypeDefinitionComposite [Member]
  | CustomTypeDefinitionEnum [EnumVariant]
  | CustomTypeDefinitionDomain Value
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "CustomTypeDefinition") CustomTypeDefinition)

-- | A custom type with name, PostgreSQL name, and definition
data CustomType = CustomType
  { name :: Name,
    pgSchema :: Text,
    pgName :: Text,
    definition :: CustomTypeDefinition
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | Result rows with cardinality and row structure
data ResultRows = ResultRows
  { cardinality :: ResultRowsCardinality,
    columns :: NonEmpty Member
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | A variable in a query fragment
data Var = Var
  { name :: Name,
    rawName :: Text,
    paramIndex :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | A fragment of a query, either SQL text or a variable
data QueryFragment
  = QueryFragmentSql Text
  | QueryFragmentVar Var
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "QueryFragment") QueryFragment)

-- | A query with name, source path, parameters, result, and fragments
data Query = Query
  { name :: Name,
    srcPath :: Path,
    params :: [Member],
    result :: Maybe ResultRows,
    fragments :: [QueryFragment]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | A project with name, version, custom types, and queries
data Project = Project
  { space :: Name,
    name :: Name,
    version :: Version,
    customTypes :: [CustomType],
    queries :: [Query]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

AesonDeriver.derive
  [ ''Version,
    ''Primitive,
    ''Scalar,
    ''ArraySettings,
    ''Value,
    ''Member,
    ''EnumVariant,
    ''CustomTypeDefinition,
    ''CustomType,
    ''ResultRowsCardinality,
    ''ResultRows,
    ''Var,
    ''QueryFragment,
    ''Query,
    ''Project
  ]
