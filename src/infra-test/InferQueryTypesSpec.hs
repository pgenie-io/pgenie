module InferQueryTypesSpec (spec) where

import Fx
import Infra.Adapters.Analyser qualified as Analyser
import Logic.Algebra
import Test.Hspec
import Utils.Prelude

spec :: Spec
spec = describe "inferQueryTypes" do
  it "fails when INSERT omits a NOT NULL column that has no default" do
    result <- runWithAnalyser do
      -- Migrations reproducing the demo's debug/1 schema
      executeMigration migration1
      executeMigration migration2
      executeMigration migration3
      executeMigration migration4
      -- Migration 5 makes 'disc' NOT NULL but the insert_album query
      -- never provides it, so any execution would violate the constraint.
      executeMigration migration5
      -- This query does not include 'disc'; after migration 5 it can never
      -- succeed and pGenie should report an error rather than silently
      -- inferring all parameters as NOT NULL.
      inferQueryTypes insertAlbumSql
    case result of
      Left _ -> pure ()
      Right _ -> expectationFailure "Expected inference to fail because disc is NOT NULL and omitted from the INSERT, but it succeeded"

-- | Run an action against a fresh throwaway PostgreSQL container.
runWithAnalyser ::
  Fx Analyser.Device Error a ->
  IO (Either Error a)
runWithAnalyser action =
  action
    & scoping (Analyser.scope (const (pure ())))
    & exposeErr
    & Fx.runFx

-- Migration 1: baseline schema (from the demo's migrations/1.sql)
migration1 :: Text
migration1 =
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
  \  primary bool not null,\
  \  primary key (album, artist)\
  \);"

-- Migration 2: widen 'id' columns to int8 (from the demo's migrations/2.sql)
migration2 :: Text
migration2 =
  "alter table album alter column id type int8;\
  \alter table album_genre alter column album type int8;\
  \alter table album_artist alter column album type int8;"

-- Migration 3: add enum/composite types and nullable columns (migrations/3.sql)
migration3 :: Text
migration3 =
  "create type album_format as enum (\
  \  'Vinyl', 'CD', 'Cassette', 'Digital', 'DVD-Audio', 'SACD'\
  \);\
  \create type recording_info as (\
  \  studio_name text, city text, country text, recorded_date date\
  \);\
  \alter table album add column format album_format null;\
  \alter table album add column recording recording_info null;"

-- Migration 4: more composite types and nullable array/composite columns (migrations/4.sql)
migration4 :: Text
migration4 =
  "create type track_info as (\
  \  title text, duration_seconds int4, tags text[]\
  \);\
  \create type disc_info as (\
  \  disc_name text, recording recording_info\
  \);\
  \alter table album add column tracks track_info[] null;\
  \alter table album add column disc disc_info null;"

-- Migration 5: make 'disc' NOT NULL without a default (migrations/5.sql).
-- After this, any INSERT that omits 'disc' will fail at runtime.
migration5 :: Text
migration5 = "alter table album alter column disc set not null;"

-- The insert_album query: inserts name/released/format/recording but NOT disc.
-- After migration 5 this is broken because disc has no default.
insertAlbumSql :: Text
insertAlbumSql =
  "insert into album (name, released, format, recording)\
  \ values ($1, $2, $3, $4)\
  \ returning id"
