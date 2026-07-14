-- |
-- Topologically sorts a project's custom types (dependencies before
-- dependents, ties broken alphabetically by pgSchema then pgName — the
-- gen-contract v5 invariant) and resolves every 'Gen.Input.CustomTypeRef'
-- throughout both the type definitions and the queries to point at the
-- sorted list's final positions. No transitive-closure computation here —
-- generators derive that themselves from the sorted order (gen-sdk's
-- CustomTypes helpers).
module Logic.Domain.CustomTypeOrdering (orderAndResolve) where

import Data.Map.Strict qualified as Map
import GenBridge.Model.Input qualified as Gen.Input
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
                      (comparing fst <> comparing snd)
                      [ k
                        | (k, ct) <- Map.toList remaining,
                          all (`Map.notMember` remaining) (dependencies ct)
                      ]
               in case ready of
                    [] -> Map.elems remaining -- cycle guard: shouldn't happen for valid custom-type graphs; emit remainder as-is rather than loop forever
                    (k : _) ->
                      let Just ct = Map.lookup k remaining
                       in ct : go (Map.delete k remaining)

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
    resolveQuery q = q {Gen.Input.params = map resolveMember q.params}
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