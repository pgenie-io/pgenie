module InferQueryTypesSpec (spec) where

import Data.Text qualified as Text
import Fx
import Infra.Adapters.Analyser qualified as Analyser
import Logic.Algebra
import PGenieGen.Model.Input qualified as Gen.Input
import Test.Hspec
import Utils.Prelude

spec :: Spec
spec = describe "inferQueryTypes" do
  it "fails when INSERT omits a NOT NULL column that has no default" do
    result <- runWithAnalyser do
      -- Migrations reproducing the demo's debug/1 schema
      executeMigration
        "create table genre (\
        \  id int4 not null generated always as identity primary key,\
        \  name text not null unique\
        \);\
        \create table artist (\
        \  id int4 not null generated always as identity primary key,\
        \  name text not null\
        \);\
        \create table album (\
        \  id int4 not null generated always as identity primary key,\
        \  name text not null,\
        \  released date null\
        \);\
        \create table album_genre (\
        \  album int4 not null references album,\
        \  genre int4 not null references genre\
        \);\
        \create table album_artist (\
        \  album int4 not null references album,\
        \  artist int4 not null references artist,\
        \  primary key (album, artist)\
        \);"

      executeMigration
        "alter table album alter column id type int8;\
        \alter table album_genre alter column album type int8;\
        \alter table album_artist alter column album type int8;"

      executeMigration
        "create type album_format as enum (\
        \  'Vinyl', 'CD', 'Cassette', 'Digital', 'DVD-Audio', 'SACD'\
        \);\
        \create type recording_info as (\
        \  studio_name text, city text, country text, recorded_date date\
        \);\
        \alter table album add column format album_format null;\
        \alter table album add column recording recording_info null;"

      executeMigration
        "create type track_info as (\
        \  title text, duration_seconds int4, tags text[]\
        \);\
        \create type disc_info as (\
        \  disc_name text, recording recording_info\
        \);\
        \alter table album add column tracks track_info[] null;\
        \alter table album add column disc disc_info null;"

      -- Migration 5 makes 'disc' NOT NULL but the insert_album query
      -- never provides it, so any execution would violate the constraint.
      executeMigration "alter table album alter column disc set not null;"

      -- This query does not include 'disc'; after migration 5 it can never
      -- succeed and pGenie should report an error rather than silently
      -- inferring all parameters as NOT NULL.
      inferQueryTypes
        "insert into album (name, released, format, recording)\
        \ values ($1, $2, $3, $4)\
        \ returning id"
    case result of
      Left err -> do
        -- The error should carry an actionable suggestion naming the missing column.
        case err.suggestion of
          Just txt | Text.isInfixOf "disc" txt -> pure ()
          _ -> expectationFailure ("Invalid suggestion. Error: " <> show err)
      Right _ ->
        expectationFailure "Expected inference to fail because disc is NOT NULL and omitted from the INSERT, but it succeeded"

  it "infers ltree, citext, and hstore types from extension-enabled queries" do
    (ltreeQueryTypes, existingExtensionQueryTypes, ltreeTableQueryTypes) <-
      expectRight =<< runWithAnalyser do
        executeMigration "create extension if not exists ltree;"
        executeMigration "create extension if not exists citext;"
        executeMigration "create extension if not exists hstore;"

        executeMigration
          "create table tree_nodes (\
          \path ltree not null,\
          \ancestors ltree[] null\
          \);"
        ltreeQueryTypes <- fst <$> inferQueryTypes "select $1::ltree as path, $2::ltree[] as path_array"
        existingExtensionQueryTypes <- fst <$> inferQueryTypes "select $1::citext as ci, 'a=>b'::hstore as attrs"
        ltreeTableQueryTypes <- fst <$> inferQueryTypes "select path, ancestors from tree_nodes"
        pure (ltreeQueryTypes, existingExtensionQueryTypes, ltreeTableQueryTypes)

    map (\param -> (param.isNullable, param.type_)) ltreeQueryTypes.params
      `shouldBe` [ (True, primitiveValue Gen.Input.PrimitiveLtree),
                   (True, arrayValue 1 Gen.Input.PrimitiveLtree)
                 ]
    map (\column -> (column.pgName, column.isNullable, column.value)) ltreeQueryTypes.resultColumns
      `shouldBe` [ ("path", True, primitiveValue Gen.Input.PrimitiveLtree),
                   ("path_array", True, arrayValue 1 Gen.Input.PrimitiveLtree)
                 ]
    ltreeQueryTypes.mentionedCustomTypes `shouldBe` []

    map (\param -> (param.isNullable, param.type_)) existingExtensionQueryTypes.params
      `shouldBe` [(True, primitiveValue Gen.Input.PrimitiveCitext)]
    map (\column -> (column.pgName, column.isNullable, column.value)) existingExtensionQueryTypes.resultColumns
      `shouldBe` [ ("ci", True, primitiveValue Gen.Input.PrimitiveCitext),
                   ("attrs", True, primitiveValue Gen.Input.PrimitiveHstore)
                 ]
    existingExtensionQueryTypes.mentionedCustomTypes `shouldBe` []

    ltreeTableQueryTypes.params `shouldBe` []
    map (\column -> (column.pgName, column.isNullable, column.value)) ltreeTableQueryTypes.resultColumns
      `shouldBe` [ ("path", False, primitiveValue Gen.Input.PrimitiveLtree),
                   ("ancestors", True, arrayValue 1 Gen.Input.PrimitiveLtree)
                 ]
    ltreeTableQueryTypes.mentionedCustomTypes `shouldBe` []

-- | Run an action against a fresh throwaway PostgreSQL container.
runWithAnalyser ::
  Fx Analyser.Device Error a ->
  IO (Either Error a)
runWithAnalyser action =
  action
    & scoping (Analyser.scope "postgres:18" (const (pure ())))
    & exposeErr
    & Fx.runFx

expectRight :: (Show err) => Either err a -> IO a
expectRight = \case
  Right value -> pure value
  Left err -> do
    expectationFailure ("Expected inference to succeed, but failed: " <> show err)
    fail "Expected successful inference"

primitiveValue :: Gen.Input.Primitive -> Gen.Input.Value
primitiveValue primitive =
  Gen.Input.Value
    { arraySettings = Nothing,
      scalar = Gen.Input.ScalarPrimitive primitive
    }

arrayValue :: Natural -> Gen.Input.Primitive -> Gen.Input.Value
arrayValue dimensionality primitive =
  Gen.Input.Value
    { arraySettings =
        Just
          Gen.Input.ArraySettings
            { dimensionality,
              elementIsNullable = True
            },
      scalar = Gen.Input.ScalarPrimitive primitive
    }
