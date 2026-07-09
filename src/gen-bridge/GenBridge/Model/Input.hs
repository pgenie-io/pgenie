module GenBridge.Model.Input
  ( module GenBridge.Model.Input,
    Name (..),
  )
where

import Dhall qualified
import GenBridge.Aeson.Deriver qualified as AesonDeriver
import GenBridge.Dhall.Deriving qualified as Dhall.Deriving
import GenBridge.Dhall.Orphans ()
import GenBridge.Model.Input.Name (Name (..))
import GenBridge.Prelude

-- | Version with semantic versioning components
data Version = Version
  { major :: Natural,
    minor :: Natural,
    patch :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | PostgreSQL primitive types.
--
-- Keep 'CharPrimitive' for the internal single-byte '"char"' type (OID 18),
-- which is distinct from 'char(n)' / 'bpchar'.
data Primitive
  = -- | PostgreSQL 'bit' (OID 1560, array OID 1561).
    BitPrimitive
  | -- | PostgreSQL 'bool' (OID 16, array OID 1000).
    BoolPrimitive
  | -- | PostgreSQL 'box' (OID 603, array OID 1020).
    BoxPrimitive
  | -- | PostgreSQL 'bpchar' / 'char(n)' (OID 1042, array OID 1014).
    BpcharPrimitive
  | -- | PostgreSQL 'bytea' (OID 17, array OID 1001).
    ByteaPrimitive
  | -- | PostgreSQL internal single-byte '"char"' (OID 18, array OID 1002).
    CharPrimitive
  | -- | PostgreSQL 'cidr' (OID 650, array OID 651).
    CidrPrimitive
  | -- | PostgreSQL 'circle' (OID 718, array OID 719).
    CirclePrimitive
  | -- | PostgreSQL 'citext' extension type (no fixed OID; requires the citext extension).
    CitextPrimitive
  | -- | PostgreSQL 'date' (OID 1082, array OID 1182).
    DatePrimitive
  | -- | PostgreSQL 'datemultirange' (OID 4535, array OID 6150).
    DatemultirangePrimitive
  | -- | PostgreSQL 'daterange' (OID 3912, array OID 3913).
    DaterangePrimitive
  | -- | PostgreSQL 'float4' / 'real' (OID 700, array OID 1021).
    Float4Primitive
  | -- | PostgreSQL 'float8' / 'double precision' (OID 701, array OID 1022).
    Float8Primitive
  | -- | PostgreSQL 'hstore' extension type (no fixed OID; requires the hstore extension).
    HstorePrimitive
  | -- | PostgreSQL 'inet' (OID 869, array OID 1041).
    InetPrimitive
  | -- | PostgreSQL 'int2' / 'smallint' (OID 21, array OID 1005).
    Int2Primitive
  | -- | PostgreSQL 'int4' / 'integer' (OID 23, array OID 1007).
    Int4Primitive
  | -- | PostgreSQL 'int4multirange' (OID 4451, array OID 6154).
    Int4multirangePrimitive
  | -- | PostgreSQL 'int4range' (OID 3904, array OID 3905).
    Int4rangePrimitive
  | -- | PostgreSQL 'int8' / 'bigint' (OID 20, array OID 1016).
    Int8Primitive
  | -- | PostgreSQL 'int8multirange' (OID 4536, array OID 6157).
    Int8multirangePrimitive
  | -- | PostgreSQL 'int8range' (OID 3926, array OID 3927).
    Int8rangePrimitive
  | -- | PostgreSQL 'interval' (OID 1186, array OID 1187).
    IntervalPrimitive
  | -- | PostgreSQL 'json' (OID 114, array OID 199).
    JsonPrimitive
  | -- | PostgreSQL 'jsonb' (OID 3802, array OID 3807).
    JsonbPrimitive
  | -- | PostgreSQL 'line' (OID 628, array OID 629).
    LinePrimitive
  | -- | PostgreSQL 'lseg' (OID 601, array OID 1018).
    LsegPrimitive
  | -- | PostgreSQL 'macaddr' (OID 829, array OID 1040).
    MacaddrPrimitive
  | -- | PostgreSQL 'macaddr8' (OID 774, array OID 775).
    Macaddr8Primitive
  | -- | PostgreSQL 'money' (OID 790, array OID 791).
    MoneyPrimitive
  | -- | PostgreSQL 'name' (OID 19, array OID 1003).
    NamePrimitive
  | -- | PostgreSQL 'numeric' (OID 1700, array OID 1231).
    NumericPrimitive
  | -- | PostgreSQL 'nummultirange' (OID 4532, array OID 6151).
    NummultirangePrimitive
  | -- | PostgreSQL 'numrange' (OID 3906, array OID 3907).
    NumrangePrimitive
  | -- | PostgreSQL 'oid' object identifier type (OID 26, array OID 1028).
    OidPrimitive
  | -- | PostgreSQL 'path' (OID 602, array OID 1019).
    PathPrimitive
  | -- | PostgreSQL 'pg_lsn' (OID 3220, array OID 3221).
    PgLsnPrimitive
  | -- | PostgreSQL 'pg_snapshot' (OID 5038, array OID 5039).
    PgSnapshotPrimitive
  | -- | PostgreSQL 'point' (OID 600, array OID 1017).
    PointPrimitive
  | -- | PostgreSQL 'polygon' (OID 604, array OID 1027).
    PolygonPrimitive
  | -- | PostgreSQL 'text' (OID 25, array OID 1009).
    TextPrimitive
  | -- | PostgreSQL 'time' (OID 1083, array OID 1183).
    TimePrimitive
  | -- | PostgreSQL 'timestamp' (OID 1114, array OID 1115).
    TimestampPrimitive
  | -- | PostgreSQL 'timestamptz' (OID 1184, array OID 1185).
    TimestamptzPrimitive
  | -- | PostgreSQL 'timetz' (OID 1266, array OID 1270).
    TimetzPrimitive
  | -- | PostgreSQL 'tsmultirange' (OID 4533, array OID 6152).
    TsmultirangePrimitive
  | -- | PostgreSQL 'tsquery' (OID 3615, array OID 3645).
    TsqueryPrimitive
  | -- | PostgreSQL 'tsrange' (OID 3908, array OID 3909).
    TsrangePrimitive
  | -- | PostgreSQL 'tstzmultirange' (OID 4534, array OID 6153).
    TstzmultirangePrimitive
  | -- | PostgreSQL 'tstzrange' (OID 3910, array OID 3911).
    TstzrangePrimitive
  | -- | PostgreSQL 'tsvector' (OID 3614, array OID 3643).
    TsvectorPrimitive
  | -- | PostgreSQL 'uuid' (OID 2950, array OID 2951).
    UuidPrimitive
  | -- | PostgreSQL 'varbit' (OID 1562, array OID 1563).
    VarbitPrimitive
  | -- | PostgreSQL 'varchar' / 'character varying' (OID 1043, array OID 1015).
    VarcharPrimitive
  | -- | PostgreSQL 'xml' (OID 142, array OID 143).
    XmlPrimitive
  | -- | PostGIS 'box2d' extension type (box2d).
    Box2DPrimitive
  | -- | PostGIS 'box3d' extension type (box3d).
    Box3DPrimitive
  | -- | PostgreSQL 'ltree' extension type (ltree).
    LtreePrimitive
  | -- | PostGIS 'geometry' extension type (geometry).
    GeometryPrimitive
  | -- | PostGIS 'geography' extension type (geography).
    GeographyPrimitive
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Primitive") Primitive)

-- | Either a primitive type or a custom type
data Scalar
  = PrimitiveScalar Primitive
  | CustomScalar Name
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Scalar") Scalar)

-- | Array settings with dimensionality and element nullability
data ArraySettings = ArraySettings
  { dimensionality :: Natural,
    elementIsNullable :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

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
  = CompositeCustomTypeDefinition [Member]
  | EnumCustomTypeDefinition [EnumVariant]
  | DomainCustomTypeDefinition Value
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

-- | Category of result rows
data ResultRowsCardinality
  = OptionalResultRowsCardinality
  | SingleResultRowsCardinality
  | MultipleResultRowsCardinality
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "ResultRowsCardinality") ResultRowsCardinality)

-- | Result rows with cardinality and row structure
data ResultRows = ResultRows
  { cardinality :: ResultRowsCardinality,
    columns :: NonEmpty Member
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | Query result classification
data Result
  = VoidResult
  | RowsAffectedResult
  | RowsResult ResultRows
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "Result") Result)

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
  = SqlQueryFragment Text
  | VarQueryFragment Var
  deriving stock (Show, Eq, Generic)
  deriving
    (Dhall.FromDhall, Dhall.ToDhall)
    via (Dhall.Deriving.Codec (Dhall.Deriving.SumModifier "QueryFragment") QueryFragment)

-- | A query with name, source path, parameters, result, and fragments
data Query = Query
  { name :: Name,
    srcPath :: Text,
    identity :: Bool,
    idempotent :: Bool,
    params :: [Member],
    result :: Result,
    fragments :: [QueryFragment]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

data Migration = Migration
  { name :: Text,
    sql :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- | A project with name, version, custom types, and queries
data Project = Project
  { space :: Name,
    name :: Name,
    version :: Version,
    customTypes :: [CustomType],
    queries :: [Query],
    migrations :: [Migration]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Dhall.ToDhall, Dhall.FromDhall)

-- * Aeson

--
-- Kebab-case field names, ObjectWithSingleField sum encoding.
-- 'Name' is hand-written in "GenBridge.Model.Input.Name", not run through
-- 'AesonDeriver.derive', but it follows the same kebab-case convention.

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
    ''Result,
    ''Var,
    ''QueryFragment,
    ''Query,
    ''Project,
    ''Migration
  ]
