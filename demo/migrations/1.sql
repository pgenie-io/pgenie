
create table "genre" (
  "id" int4 not null generated always as identity primary key,
  "name" text not null unique
);

create table "artist" (
  "id" int4 not null generated always as identity primary key,
  "name" text not null
);

create table "album" (
  "id" int4 not null generated always as identity primary key,
  -- Album name.
  "name" text not null,
  -- The date the album was first released.
  "released" date null
);

create table "album_genre" (
  "album" int4 not null references "album",
  "genre" int4 not null references "genre"
);

create table "album_artist" (
  "album" int4 not null references "album",
  "artist" int4 not null references "artist",
  -- Whether it is the primary artist
  "primary" bool not null,
  primary key ("album", "artist")
);
