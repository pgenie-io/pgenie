module MySpace.MusicCatalogue.Statements.SelectAlbumsWithRecordingInfo where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @select_albums_with_recording_info@ query.
--
-- ==== SQL Template
--
-- > -- Select albums with their full recording information
-- > select 
-- >   album.id,
-- >   album.name,
-- >   album.released,
-- >   album.format,
-- >   album.recording,
-- >   (album.recording).studio_name as studio_name,
-- >   (album.recording).city as recording_city
-- > from album
-- > where album.recording is not null
-- > order by (album.recording).recorded_date desc
--
-- ==== Source Path
--
-- > ./queries/select_albums_with_recording_info.sql
--
data SelectAlbumsWithRecordingInfo = SelectAlbumsWithRecordingInfo
  deriving stock (Eq, Show)

-- | Result of the statement parameterised by 'SelectAlbumsWithRecordingInfo'.
type SelectAlbumsWithRecordingInfoResult = Vector.Vector SelectAlbumsWithRecordingInfoResultRow

-- | Row of 'SelectAlbumsWithRecordingInfoResult'.
data SelectAlbumsWithRecordingInfoResultRow = SelectAlbumsWithRecordingInfoResultRow
  { -- | Maps to @id@.
    id :: Int64,
    -- | Maps to @name@.
    name :: Text,
    -- | Maps to @released@.
    released :: Maybe (Day),
    -- | Maps to @format@.
    format :: Maybe (Types.AlbumFormat),
    -- | Maps to @recording@.
    recording :: Maybe (Types.RecordingInfo),
    -- | Maps to @studio_name@.
    studioName :: Maybe (Text),
    -- | Maps to @recording_city@.
    recordingCity :: Maybe (Text)
  }
  deriving stock (Show, Eq)


instance Mapping.IsStatement SelectAlbumsWithRecordingInfo where
  type Result SelectAlbumsWithRecordingInfo = SelectAlbumsWithRecordingInfoResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "-- Select albums with their full recording information\n\
        \select \n\
        \  album.id,\n\
        \  album.name,\n\
        \  album.released,\n\
        \  album.format,\n\
        \  album.recording,\n\
        \  (album.recording).studio_name as studio_name,\n\
        \  (album.recording).city as recording_city\n\
        \from album\n\
        \where album.recording is not null\n\
        \order by (album.recording).recorded_date desc"

      encoder =
        mconcat
          [ 
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          released <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          format <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          recording <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          studioName <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          recordingCity <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          pure SelectAlbumsWithRecordingInfoResultRow {..}

