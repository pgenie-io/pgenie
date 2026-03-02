module MySpace.MusicCatalogue.Types.RecordingInfo where

import MySpace.MusicCatalogue.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Mapping as Mapping

-- |
-- Representation of the @recording_info@ user-declared PostgreSQL record type.
data RecordingInfo = RecordingInfo
  { -- | Maps to @studio_name@.
    studioName :: Maybe (Text),
    -- | Maps to @city@.
    city :: Maybe (Text),
    -- | Maps to @country@.
    country :: Maybe (Text),
    -- | Maps to @recorded_date@.
    recordedDate :: Maybe (Day)
  }
  deriving stock (Show, Eq, Ord)

instance Mapping.IsScalar RecordingInfo where
  scalarEncoder =
    Encoders.composite
      (Just "public")
      "recording_info"
      ( mconcat
          [ (.studioName) >$< Encoders.field (Encoders.nullable (Mapping.scalarEncoder)),
            (.city) >$< Encoders.field (Encoders.nullable (Mapping.scalarEncoder)),
            (.country) >$< Encoders.field (Encoders.nullable (Mapping.scalarEncoder)),
            (.recordedDate) >$< Encoders.field (Encoders.nullable (Mapping.scalarEncoder))
          ]
      )
  
  scalarDecoder =
    Decoders.composite
      (Just "public")
      "recording_info"
      ( RecordingInfo
          <$> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
      )
  
