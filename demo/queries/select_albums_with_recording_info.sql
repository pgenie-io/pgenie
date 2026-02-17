-- Select albums with their full recording information
select 
  album.id,
  album.name,
  album.released,
  album.format,
  album.recording,
  (album.recording).studio_name as studio_name,
  (album.recording).city as recording_city
from album
where album.recording is not null
order by (album.recording).recorded_date desc
