insert into album (name, released, format, recording)
values ($name, $released, $format, $recording)
returning id
