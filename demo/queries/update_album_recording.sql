-- Update album recording information
update album
set recording = row($studio_name, $city, $country, $recorded_date)::recording_info
where id = $id
returning *
