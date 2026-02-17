module MySpace.MusicCatalogue.Types.RecordingInfo where

import MySpace.MusicCatalogue.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Mapping as Mapping
import qualified Hasql.Mapping as Mapping

-- |
-- Representation of the @recording_info@ user-declared PostgreSQL record type.
data RecordingInfo = RecordingInfo
  { -- | Maps to @studio_name@.
    studioName :: Text,
    -- | Maps to @city@.
    city :: Text,
    -- | Maps to @country@.
    country :: Text,
    -- | Maps to @recorded_date@.
    recordedDate :: Day
  }
  deriving stock (Show, Eq, Ord)

instance Mapping.IsScalar RecordingInfo where
  scalarEncoder =
    Encoders.composite
      (Just "public")
      "recording_info"
      ( mconcat
          [ (.studioName) >$< Encoders.field (Encoders.nonNullable (Mapping.scalarEncoder)),
            (.city) >$< Encoders.field (Encoders.nonNullable (Mapping.scalarEncoder)),
            (.country) >$< Encoders.field (Encoders.nonNullable (Mapping.scalarEncoder)),
            (.recordedDate) >$< Encoders.field (Encoders.nonNullable (Mapping.scalarEncoder))
          ]
      )
  
  scalarDecoder =
    Decoders.composite
      (Just "public")
      "recording_info"
      ( RecordingInfo
          <$> Decoders.field (Decoders.nonNullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nonNullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nonNullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nonNullable (Mapping.scalarDecoder))
      )
  
