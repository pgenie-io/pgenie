-- Composite with array fields: a track in an album
create type track_info as (
  title text,
  duration_seconds int4,
  tags text[]
);

-- Composite with composites: disc information referencing a recording
create type disc_info as (
  name text,
  recording recording_info
);

-- Edge case: array of composites column
alter table album
add column tracks track_info[] null;

-- Edge case: composite with composites column
alter table album
add column disc disc_info null;
