-- Add enumeration type for album formats
create type album_format as enum (
  'Vinyl',
  'CD',
  'Cassette',
  'Digital',
  'DVD-Audio',
  'SACD'
);

-- Add composite type for recording session information
create type recording_info as (
  studio_name text,
  city text,
  country text,
  recorded_date date
);

-- Add format column to album table
alter table album
add column format album_format null;

-- Add recording information to album table
alter table album
add column recording recording_info null;
