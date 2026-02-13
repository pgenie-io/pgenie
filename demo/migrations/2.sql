-- In this migration we're changing the type of the album "id" column
-- from "int4" to "int8".
-- Since this column is referenced from other tables, we also update them.


alter table album
alter column id type int8;

alter table album_genre
alter column album type int8;

alter table album_artist
alter column album type int8;
