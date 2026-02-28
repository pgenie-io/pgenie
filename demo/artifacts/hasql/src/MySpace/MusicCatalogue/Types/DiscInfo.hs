module MySpace.MusicCatalogue.Types.DiscInfo where

import MySpace.MusicCatalogue.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Mapping as Mapping
import qualified Hasql.Mapping as Mapping
import MySpace.MusicCatalogue.Types.RecordingInfo (RecordingInfo)

-- |
-- Representation of the @disc_info@ user-declared PostgreSQL record type.
--
-- Demonstrates a composite type with a composite field (@recording recording_info@).
data DiscInfo = DiscInfo
  { -- | Maps to @name@.
    name :: Maybe (Text),
    -- | Maps to @recording@.
    recording :: Maybe (RecordingInfo)
  }
  deriving stock (Show, Eq, Ord)

instance Mapping.IsScalar DiscInfo where
  scalarEncoder =
    Encoders.composite
      (Just "public")
      "disc_info"
      ( mconcat
          [ (.name) >$< Encoders.field (Encoders.nullable (Mapping.scalarEncoder)),
            (.recording) >$< Encoders.field (Encoders.nullable (Mapping.scalarEncoder))
          ]
      )
  
  scalarDecoder =
    Decoders.composite
      (Just "public")
      "disc_info"
      ( DiscInfo
          <$> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
      )
