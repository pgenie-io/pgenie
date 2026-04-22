select id, genre.name
from genre
left join album_genre on album_genre.genre = genre.id
left join album_artist on album_artist.album = album_genre.album
where album_artist.artist = $artist
