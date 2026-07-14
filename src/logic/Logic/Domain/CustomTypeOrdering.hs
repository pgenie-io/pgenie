-- |
-- Topologically sorts a project's custom types (dependencies before
-- dependents, ties broken alphabetically by pgSchema then pgName — the
-- gen-contract v5 invariant) and resolves every 'Gen.Input.CustomTypeRef'
-- throughout both the type definitions and the queries to point at the
-- sorted list's final positions. No transitive-closure computation here —
-- generators derive that themselves from the sorted order (gen-sdk's
-- CustomTypes helpers).
module Logic.Domain.CustomTypeOrdering (orderAndResolve, spec) where

import Data.Map.Strict qualified as Map
import GenBridge.Model.Input qualified as Gen.Input
import Test.Hspec
import Utils.Prelude

type Key = (Text, Text)

orderAndResolve ::
  [Gen.Input.CustomType] ->
  [Gen.Input.Query] ->
  ([Gen.Input.CustomType], [Gen.Input.Query])
orderAndResolve customTypes queries =
  (sortedResolved, map resolveQuery queries)
  where
    key :: Gen.Input.CustomType -> Key
    key ct = (ct.pgSchema, ct.pgName)

    dependencies :: Gen.Input.CustomType -> [Key]
    dependencies ct = case ct.definition of
      Gen.Input.CompositeCustomTypeDefinition members ->
        [k | m <- members, Just k <- [refKey m.value]]
      Gen.Input.EnumCustomTypeDefinition _ -> []
      Gen.Input.DomainCustomTypeDefinition value ->
        [k | Just k <- [refKey value]]

    refKey :: Gen.Input.Value -> Maybe Key
    refKey value = case value.scalar of
      Gen.Input.CustomScalar ref -> Just (ref.pgSchema, ref.pgName)
      Gen.Input.PrimitiveScalar _ -> Nothing

    -- Kahn's algorithm: repeatedly peel off the alphabetically-first node
    -- among those whose dependencies have already been emitted.
    sorted :: [Gen.Input.CustomType]
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

    resolveRef :: Gen.Input.CustomTypeRef -> Gen.Input.CustomTypeRef
    resolveRef ref =
      ref {Gen.Input.index = Map.findWithDefault ref.index (ref.pgSchema, ref.pgName) indexOf}

    resolveValue :: Gen.Input.Value -> Gen.Input.Value
    resolveValue value = value {Gen.Input.scalar = resolveScalar value.scalar}

    resolveScalar :: Gen.Input.Scalar -> Gen.Input.Scalar
    resolveScalar (Gen.Input.CustomScalar ref) = Gen.Input.CustomScalar (resolveRef ref)
    resolveScalar prim = prim

    resolveMember :: Gen.Input.Member -> Gen.Input.Member
    resolveMember m = m {Gen.Input.value = resolveValue m.value}

    resolveCustomType :: Gen.Input.CustomType -> Gen.Input.CustomType
    resolveCustomType ct =
      ct
        { Gen.Input.definition = case ct.definition of
            Gen.Input.CompositeCustomTypeDefinition members ->
              Gen.Input.CompositeCustomTypeDefinition (map resolveMember members)
            Gen.Input.EnumCustomTypeDefinition variants ->
              Gen.Input.EnumCustomTypeDefinition variants
            Gen.Input.DomainCustomTypeDefinition value ->
              Gen.Input.DomainCustomTypeDefinition (resolveValue value)
        }

    sortedResolved = map resolveCustomType sorted

    resolveQuery :: Gen.Input.Query -> Gen.Input.Query
    resolveQuery q =
      q {Gen.Input.params = map resolveMember q.params}
        & resolveQueryResult

    resolveQueryResult :: Gen.Input.Query -> Gen.Input.Query
    resolveQueryResult q =
      q {Gen.Input.result = resolveResult q.result}

    resolveResult :: Gen.Input.Result -> Gen.Input.Result
    resolveResult = \case
      Gen.Input.VoidResult -> Gen.Input.VoidResult
      Gen.Input.RowsAffectedResult -> Gen.Input.RowsAffectedResult
      Gen.Input.RowsResult rows ->
        Gen.Input.RowsResult
          rows {Gen.Input.columns = fmap resolveMember rows.columns}

-- * Tests

spec :: Spec
spec = do
  describe "orderAndResolve" do
    it "sorts domain-before-composite when supplied in reverse dependency order" do
      let domainType =
            Gen.Input.CustomType
              { name = textName "temp_celsius",
                pgSchema = "public",
                pgName = "temp_celsius",
                definition =
                  Gen.Input.DomainCustomTypeDefinition
                    $ Gen.Input.Value
                      { dimensionality = 0,
                        elementIsNullable = False,
                        scalar = Gen.Input.PrimitiveScalar Gen.Input.Float8Primitive
                      }
              }
          compositeType =
            Gen.Input.CustomType
              { name = textName "weather_reading",
                pgSchema = "public",
                pgName = "weather_reading",
                definition =
                  Gen.Input.CompositeCustomTypeDefinition
                    [ Gen.Input.Member
                        { name = textName "temperature",
                          pgName = "temperature",
                          isNullable = False,
                          value =
                            Gen.Input.Value
                              { dimensionality = 0,
                                elementIsNullable = False,
                                scalar =
                                  Gen.Input.CustomScalar
                                    $ Gen.Input.CustomTypeRef
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
            Gen.Input.CustomType
              { name = textName "temp_celsius",
                pgSchema = "public",
                pgName = "temp_celsius",
                definition =
                  Gen.Input.DomainCustomTypeDefinition
                    $ Gen.Input.Value
                      { dimensionality = 0,
                        elementIsNullable = False,
                        scalar = Gen.Input.PrimitiveScalar Gen.Input.Float8Primitive
                      }
              }
          compositeType =
            Gen.Input.CustomType
              { name = textName "weather_reading",
                pgSchema = "public",
                pgName = "weather_reading",
                definition =
                  Gen.Input.CompositeCustomTypeDefinition
                    [ Gen.Input.Member
                        { name = textName "temperature",
                          pgName = "temperature",
                          isNullable = False,
                          value =
                            Gen.Input.Value
                              { dimensionality = 0,
                                elementIsNullable = False,
                                scalar =
                                  Gen.Input.CustomScalar
                                    $ Gen.Input.CustomTypeRef
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
            Gen.Input.CustomType
              { definition = Gen.Input.CompositeCustomTypeDefinition [Gen.Input.Member {value = Gen.Input.Value {scalar = Gen.Input.CustomScalar ref}}]
              } -> ref
            _ -> error "Unexpected shape"
      resolvedRef.index `shouldBe` 0

    it "resolves indices in queries" do
      let domainType =
            Gen.Input.CustomType
              { name = textName "temp_celsius",
                pgSchema = "public",
                pgName = "temp_celsius",
                definition =
                  Gen.Input.DomainCustomTypeDefinition
                    $ Gen.Input.Value
                      { dimensionality = 0,
                        elementIsNullable = False,
                        scalar = Gen.Input.PrimitiveScalar Gen.Input.Float8Primitive
                      }
              }
          query =
            Gen.Input.Query
              { name = textName "get_temp",
                srcPath = "queries/get_temp.sql",
                identity = False,
                idempotent = True,
                params = [],
                result =
                  Gen.Input.RowsResult
                    $ Gen.Input.ResultRows
                      { cardinality = Gen.Input.OptionalResultRowsCardinality,
                        columns =
                          Gen.Input.Member
                            { name = textName "temp",
                              pgName = "temp",
                              isNullable = False,
                              value =
                                Gen.Input.Value
                                  { dimensionality = 0,
                                    elementIsNullable = False,
                                    scalar =
                                      Gen.Input.CustomScalar
                                        $ Gen.Input.CustomTypeRef
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
            Gen.Input.Query
              { result = Gen.Input.RowsResult Gen.Input.ResultRows {columns = Gen.Input.Member {value = Gen.Input.Value {scalar = Gen.Input.CustomScalar ref}} :| _}
              } -> ref
            _ -> error "Unexpected shape"
      resolvedRef.index `shouldBe` 0

    it "sorts alphabetically when no dependencies exist" do
      let typeA =
            Gen.Input.CustomType
              { name = textName "beta",
                pgSchema = "public",
                pgName = "beta",
                definition = Gen.Input.EnumCustomTypeDefinition []
              }
          typeB =
            Gen.Input.CustomType
              { name = textName "alpha",
                pgSchema = "public",
                pgName = "alpha",
                definition = Gen.Input.EnumCustomTypeDefinition []
              }
          (sorted, _) = orderAndResolve [typeB, typeA] []
      map (.pgName) sorted `shouldBe` ["alpha", "beta"]

textName :: Text -> Gen.Input.Name
textName source =
  Gen.Input.Name
    { inCamelCase = source,
      inPascalCase = source,
      inKebabCase = source,
      inTrainCase = source,
      inScreamingKebabCase = source,
      inSnakeCase = source,
      inCamelSnakeCase = source,
      inScreamingSnakeCase = source
    }
