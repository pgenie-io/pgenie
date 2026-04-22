create type track_info as (
  title text,
  duration_seconds int4,
  tags text[]
);

create type disc_info as (
  name text,
  recording recording_info
);

alter table album add column tracks track_info[] null;
alter table album add column disc disc_info null;
