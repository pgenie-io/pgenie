select 
  id,
  name,
  released,
  format,
  recording
from album
where format = $format
