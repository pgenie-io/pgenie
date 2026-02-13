update album
set released = $released
where id = $id
returning *
