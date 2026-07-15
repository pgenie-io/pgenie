-- |
-- Topologically sorts a project's custom types (dependencies before
-- dependents, ties broken alphabetically by pgSchema then pgName — the
-- gen-contract v5 invariant) and resolves every 'Gen.CustomTypeRef'
-- throughout both the type definitions and the queries to point at the
-- sorted list's final positions. No transitive-closure computation here —
-- generators derive that themselves from the sorted order (gen-sdk's
-- CustomTypes helpers).
module Logic.Domain.CustomTypeOrdering (orderAndResolve, spec) where

import Data.Map.Strict qualified as Map
import GenBridge.Contract qualified as Gen
import Test.Hspec
import Utils.Prelude

type Key = (Text, Text)

orderAndResolve ::
  [Gen.CustomType] ->
  [Gen.Query] ->
  ([Gen.CustomType], [Gen.Query])
orderAndResolve customTypes queries =
  (sortedResolved, map resolveQuery queries)
  where
    key :: Gen.CustomType -> Key
    key ct = (ct.pgSchema, ct.pgName)

    dependencies :: Gen.CustomType -> [Key]
    dependencies ct = case ct.definition of
      Gen.CompositeCustomTypeDefinition members ->
        [k | m <- members, Just k <- [refKey m.value]]
      Gen.EnumCustomTypeDefinition _ -> []
      Gen.DomainCustomTypeDefinition value ->
        [k | Just k <- [refKey value]]

    refKey :: Gen.Value -> Maybe Key
    refKey value = case value.scalar of
      Gen.CustomScalar ref -> Just (ref.pgSchema, ref.pgName)
      Gen.PrimitiveScalar _ -> Nothing

    -- Kahn's algorithm: repeatedly peel off the alphabetically-first node
    -- among those whose dependencies have already been emitted.
    sorted :: [Gen.CustomType]
    sorted = go (Map.fromList [(key ct, ct) | ct <- customTypes])
      where
        go remaining
          | Map.null remaining = []
          | otherwise =
              let ready =
                    sortBy
                      (comparing fst)
                      [ (k, ct)
                      | (k, ct) <- Map.toList remaining,
                        all (`Map.notMember` remaining) (dependencies ct)
                      ]
               in case ready of
                    [] -> Map.elems remaining -- cycle guard: shouldn't happen for valid custom-type graphs; emit remainder as-is rather than loop forever
                    ((k, ct) : _) -> ct : go (Map.delete k remaining)

    indexOf :: Map Key Natural
    indexOf = Map.fromList (zip (map key sorted) [0 ..])

    resolveRef :: Gen.CustomTypeRef -> Gen.CustomTypeRef
    resolveRef ref =
      ref {Gen.index = Map.findWithDefault ref.index (ref.pgSchema, ref.pgName) indexOf}

    resolveValue :: Gen.Value -> Gen.Value
    resolveValue value = value {Gen.scalar = resolveScalar value.scalar}

    resolveScalar :: Gen.Scalar -> Gen.Scalar
    resolveScalar (Gen.CustomScalar ref) = Gen.CustomScalar (resolveRef ref)
    resolveScalar prim = prim

    resolveMember :: Gen.Member -> Gen.Member
    resolveMember Gen.Member {..} = Gen.Member {value = resolveValue value, ..}

    resolveCustomType :: Gen.CustomType -> Gen.CustomType
    resolveCustomType ct =
      ct
        { Gen.definition = case ct.definition of
            Gen.CompositeCustomTypeDefinition members ->
              Gen.CompositeCustomTypeDefinition (map resolveMember members)
            Gen.EnumCustomTypeDefinition variants ->
              Gen.EnumCustomTypeDefinition variants
            Gen.DomainCustomTypeDefinition value ->
              Gen.DomainCustomTypeDefinition (resolveValue value)
        }

    sortedResolved = map resolveCustomType sorted

    resolveQuery :: Gen.Query -> Gen.Query
    resolveQuery q =
      q {Gen.params = map resolveMember q.params}
        & resolveQueryResult

    resolveQueryResult :: Gen.Query -> Gen.Query
    resolveQueryResult q =
      q {Gen.result = resolveResult q.result}

    resolveResult :: Gen.Result -> Gen.Result
    resolveResult = \case
      Gen.VoidResult -> Gen.VoidResult
      Gen.RowsAffectedResult -> Gen.RowsAffectedResult
      Gen.RowsResult rows ->
        Gen.RowsResult
          rows {Gen.columns = fmap resolveMember rows.columns}

-- * Tests

spec :: Spec
spec = do
  describe "orderAndResolve" do
    it "sorts domain-before-composite when supplied in reverse dependency order" do
      let domainType =
            Gen.CustomType
              { name = textName "temp_celsius",
                pgSchema = "public",
                pgName = "temp_celsius",
                definition =
                  Gen.DomainCustomTypeDefinition
                    $ Gen.Value
                      { dimensionality = 0,
                        elementIsNullable = False,
                        scalar = Gen.PrimitiveScalar Gen.Float8Primitive
                      }
              }
          compositeType =
            Gen.CustomType
              { name = textName "weather_reading",
                pgSchema = "public",
                pgName = "weather_reading",
                definition =
                  Gen.CompositeCustomTypeDefinition
                    [ Gen.Member
                        { name = textName "temperature",
                          pgName = "temperature",
                          isNullable = False,
                          value =
                            Gen.Value
                              { dimensionality = 0,
                                elementIsNullable = False,
                                scalar =
                                  Gen.CustomScalar
                                    $ Gen.CustomTypeRef
                                      { name = textName "temp_celsius",
                                        pgSchema = "public",
                                        pgName = "temp_celsius",
                                        index = 0
                                      }
                              }
                        }
                    ]
              }
          (sorted, _) = orderAndResolve [compositeType, domainType] []
      sorted `shouldBe` [domainType, compositeType]

    it "rewrites indices to match sorted positions" do
      let domainType =
            Gen.CustomType
              { name = textName "temp_celsius",
                pgSchema = "public",
                pgName = "temp_celsius",
                definition =
                  Gen.DomainCustomTypeDefinition
                    $ Gen.Value
                      { dimensionality = 0,
                        elementIsNullable = False,
                        scalar = Gen.PrimitiveScalar Gen.Float8Primitive
                      }
              }
          compositeType =
            Gen.CustomType
              { name = textName "weather_reading",
                pgSchema = "public",
                pgName = "weather_reading",
                definition =
                  Gen.CompositeCustomTypeDefinition
                    [ Gen.Member
                        { name = textName "temperature",
                          pgName = "temperature",
                          isNullable = False,
                          value =
                            Gen.Value
                              { dimensionality = 0,
                                elementIsNullable = False,
                                scalar =
                                  Gen.CustomScalar
                                    $ Gen.CustomTypeRef
                                      { name = textName "temp_celsius",
                                        pgSchema = "public",
                                        pgName = "temp_celsius",
                                        index = 0
                                      }
                              }
                        }
                    ]
              }
          (sorted, _) = orderAndResolve [compositeType, domainType] []
          resolvedRef = case sorted !! 1 of
            Gen.CustomType
              { definition = Gen.CompositeCustomTypeDefinition [Gen.Member {value = Gen.Value {scalar = Gen.CustomScalar ref}}]
              } -> ref
            _ -> error "Unexpected shape"
      resolvedRef.index `shouldBe` 0

    it "resolves indices in queries" do
      let domainType =
            Gen.CustomType
              { name = textName "temp_celsius",
                pgSchema = "public",
                pgName = "temp_celsius",
                definition =
                  Gen.DomainCustomTypeDefinition
                    $ Gen.Value
                      { dimensionality = 0,
                        elementIsNullable = False,
                        scalar = Gen.PrimitiveScalar Gen.Float8Primitive
                      }
              }
          query =
            Gen.Query
              { name = textName "get_temp",
                srcPath = "queries/get_temp.sql",
                identity = False,
                idempotent = True,
                params = [],
                result =
                  Gen.RowsResult
                    $ Gen.ResultRows
                      { cardinality = Gen.OptionalResultRowsCardinality,
                        columns =
                          Gen.Member
                            { name = textName "temp",
                              pgName = "temp",
                              isNullable = False,
                              value =
                                Gen.Value
                                  { dimensionality = 0,
                                    elementIsNullable = False,
                                    scalar =
                                      Gen.CustomScalar
                                        $ Gen.CustomTypeRef
                                          { name = textName "temp_celsius",
                                            pgSchema = "public",
                                            pgName = "temp_celsius",
                                            index = 0
                                          }
                                  }
                            }
                            :| []
                      },
                fragments = []
              }
          (_, resolvedQueries) = orderAndResolve [domainType] [query]
          resolvedRef = case resolvedQueries !! 0 of
            Gen.Query
              { result = Gen.RowsResult Gen.ResultRows {columns = Gen.Member {value = Gen.Value {scalar = Gen.CustomScalar ref}} :| _}
              } -> ref
            _ -> error "Unexpected shape"
      resolvedRef.index `shouldBe` 0

    it "sorts alphabetically when no dependencies exist" do
      let typeA =
            Gen.CustomType
              { name = textName "beta",
                pgSchema = "public",
                pgName = "beta",
                definition = Gen.EnumCustomTypeDefinition []
              }
          typeB =
            Gen.CustomType
              { name = textName "alpha",
                pgSchema = "public",
                pgName = "alpha",
                definition = Gen.EnumCustomTypeDefinition []
              }
          (sorted, _) = orderAndResolve [typeB, typeA] []
      map (.pgName) sorted `shouldBe` ["alpha", "beta"]

textName :: Text -> Gen.Name
textName source =
  Gen.Name
    { inCamelCase = source,
      inPascalCase = source,
      inKebabCase = source,
      inTrainCase = source,
      inScreamingKebabCase = source,
      inSnakeCase = source,
      inCamelSnakeCase = source,
      inScreamingSnakeCase = source
    }
