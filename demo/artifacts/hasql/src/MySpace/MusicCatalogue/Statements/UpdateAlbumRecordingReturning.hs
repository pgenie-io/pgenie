module MySpace.MusicCatalogue.Statements.UpdateAlbumRecordingReturning where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @update_album_recording_returning@ query.
--
-- ==== SQL Template
--
-- > -- Update album recording information
-- > update album
-- > set recording = $recording
-- > where id = $id
-- > returning *
--
-- ==== Source Path
--
-- > ./queries/update_album_recording_returning.sql
--
data UpdateAlbumRecordingReturning = UpdateAlbumRecordingReturning
  { -- | Maps to @recording@.
    recording :: Maybe (Types.RecordingInfo),
    -- | Maps to @id@.
    id :: Maybe (Int64)
  }
  deriving stock (Eq, Show)

-- | Result of the statement parameterised by 'UpdateAlbumRecordingReturning'.
type UpdateAlbumRecordingReturningResult = Vector.Vector UpdateAlbumRecordingReturningResultRow

-- | Row of 'UpdateAlbumRecordingReturningResult'.
data UpdateAlbumRecordingReturningResultRow = UpdateAlbumRecordingReturningResultRow
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
    -- | Maps to @tracks@.
    tracks :: Maybe (Vector (Maybe (Types.TrackInfo))),
    -- | Maps to @disc@.
    disc :: Maybe (Types.DiscInfo)
  }
  deriving stock (Show, Eq)


instance Mapping.IsStatement UpdateAlbumRecordingReturning where
  type Result UpdateAlbumRecordingReturning = UpdateAlbumRecordingReturningResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "-- Update album recording information\n\
        \update album\n\
        \set recording = $1\n\
        \where id = $2\n\
        \returning *"

      encoder =
        mconcat
          [ (.recording) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder)),
            (.id) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          released <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          format <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          recording <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          tracks <- Decoders.column (Decoders.nullable (Decoders.array (Decoders.dimension Vector.replicateM (Decoders.element (Decoders.nullable (Mapping.scalarDecoder))))))
          disc <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          pure UpdateAlbumRecordingReturningResultRow {..}

