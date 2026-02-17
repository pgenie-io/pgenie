-- Find all vinyl albums recorded in a specific country
select 
  album.id,
  album.name,
  album.released,
  album.format,
  (album.recording).studio_name as studio,
  (album.recording).city as city,
  (album.recording).country as country,
  (album.recording).recorded_date as recorded_date
from album
where 
  album.format = 'Vinyl'
  and (album.recording).country = $country
order by (album.recording).recorded_date
