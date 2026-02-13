select album.*
from album
left join album_artist on album_artist.album = album.id
where artist = $artist
