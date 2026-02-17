select 
  album.id,
  album.name,
  album.released,
  album.format,
  (album.recording).studio_name as studio,
  (album.recording).city as recording_city
from album
left join album_artist on album_artist.album = album.id
where artist = $artist
