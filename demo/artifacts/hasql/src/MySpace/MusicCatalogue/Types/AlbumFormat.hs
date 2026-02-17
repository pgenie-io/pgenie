module MySpace.MusicCatalogue.Types.AlbumFormat where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Mapping as Mapping

-- |
-- Representation of the @album_format@ user-declared PostgreSQL enumeration type.
data AlbumFormat
  = -- | Corresponds to the PostgreSQL enum variant @Vinyl@.
    VinylAlbumFormat
  | -- | Corresponds to the PostgreSQL enum variant @CD@.
    CdAlbumFormat
  | -- | Corresponds to the PostgreSQL enum variant @Cassette@.
    CassetteAlbumFormat
  | -- | Corresponds to the PostgreSQL enum variant @Digital@.
    DigitalAlbumFormat
  | -- | Corresponds to the PostgreSQL enum variant @DVD-Audio@.
    DvdAudioAlbumFormat
  | -- | Corresponds to the PostgreSQL enum variant @SACD@.
    SacdAlbumFormat
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Mapping.IsScalar AlbumFormat where
  scalarEncoder =
    Encoders.enum
      (Just "public")
      "album_format"
      ( \case
          VinylAlbumFormat -> "Vinyl"
          CdAlbumFormat -> "CD"
          CassetteAlbumFormat -> "Cassette"
          DigitalAlbumFormat -> "Digital"
          DvdAudioAlbumFormat -> "DVD-Audio"
          SacdAlbumFormat -> "SACD"
      )
  
  scalarDecoder =
    Decoders.enum
      (Just "public")
      "album_format"
      ( \case
          "Vinyl" -> Just VinylAlbumFormat
          "CD" -> Just CdAlbumFormat
          "Cassette" -> Just CassetteAlbumFormat
          "Digital" -> Just DigitalAlbumFormat
          "DVD-Audio" -> Just DvdAudioAlbumFormat
          "SACD" -> Just SacdAlbumFormat
          _ -> Nothing
      )
