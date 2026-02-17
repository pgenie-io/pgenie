-- Update album recording information
update album
set recording = $recording
where id = $id
returning *
