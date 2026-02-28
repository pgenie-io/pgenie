select id, name, released, format
from album
where released = $released
