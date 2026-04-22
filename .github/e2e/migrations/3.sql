create type album_format as enum (
  'Vinyl',
  'CD',
  'Cassette',
  'Digital',
  'DVD-Audio',
  'SACD'
);

create type recording_info as (
  studio_name text,
  city text,
  country text,
  recorded_date date
);

alter table album add column format album_format null;
alter table album add column recording recording_info null;
