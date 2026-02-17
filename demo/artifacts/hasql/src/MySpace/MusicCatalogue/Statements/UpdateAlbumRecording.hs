module MySpace.MusicCatalogue.Statements.UpdateAlbumRecording where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @update_album_recording@ query.
--
-- ==== SQL Template
--
-- > -- Update album recording information
-- > update album
-- > set recording = row($studio_name, $city, $country, $recorded_date)::recording_info
-- > where id = $id
-- > returning *
--
-- ==== Source Path
--
-- > ./queries/update_album_recording.sql
--
data UpdateAlbumRecording = UpdateAlbumRecording
  { -- | Maps to @studio_name@.
    studioName :: Maybe (Text),
    -- | Maps to @city@.
    city :: Maybe (Text),
    -- | Maps to @country@.
    country :: Maybe (Text),
    -- | Maps to @recorded_date@.
    recordedDate :: Maybe (Day),
    -- | Maps to @id@.
    id :: Maybe (Int64)
  }
  deriving stock (Eq, Show)

-- | Result of the statement parameterised by 'UpdateAlbumRecording'.
type UpdateAlbumRecordingResult = Vector.Vector UpdateAlbumRecordingResultRow

-- | Row of 'UpdateAlbumRecordingResult'.
data UpdateAlbumRecordingResultRow = UpdateAlbumRecordingResultRow
  { -- | Maps to @id@.
    id :: Int64,
    -- | Maps to @name@.
    name :: Text,
    -- | Maps to @released@.
    released :: Maybe (Day),
    -- | Maps to @format@.
    format :: Maybe (Types.AlbumFormat),
    -- | Maps to @recording@.
    recording :: Maybe (Types.RecordingInfo)
  }
  deriving stock (Show, Eq)


instance Mapping.IsStatement UpdateAlbumRecording where
  type Result UpdateAlbumRecording = UpdateAlbumRecordingResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "-- Update album recording information\n\
        \update album\n\
        \set recording = row($1, $2, $3, $4)::recording_info\n\
        \where id = $5\n\
        \returning *"

      encoder =
        mconcat
          [ (.studioName) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder)),
            (.city) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder)),
            (.country) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder)),
            (.recordedDate) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder)),
            (.id) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          released <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          format <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          recording <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          pure UpdateAlbumRecordingResultRow {..}

