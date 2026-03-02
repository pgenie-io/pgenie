module MySpace.MusicCatalogue.Types.TrackInfo where

import MySpace.MusicCatalogue.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Mapping as Mapping

-- |
-- Representation of the @track_info@ user-declared PostgreSQL record type.
data TrackInfo = TrackInfo
  { -- | Maps to @title@.
    title :: Maybe (Text),
    -- | Maps to @duration_seconds@.
    durationSeconds :: Maybe (Int32),
    -- | Maps to @tags@.
    tags :: Maybe (Vector (Maybe Text))
  }
  deriving stock (Show, Eq, Ord)

instance Mapping.IsScalar TrackInfo where
  scalarEncoder =
    Encoders.composite
      (Just "public")
      "track_info"
      ( mconcat
          [ (.title) >$< Encoders.field (Encoders.nullable (Mapping.scalarEncoder)),
            (.durationSeconds) >$< Encoders.field (Encoders.nullable (Mapping.scalarEncoder)),
            (.tags) >$< Encoders.field (Encoders.nullable (Encoders.array (Encoders.dimension Vector.foldl' (Encoders.element (Encoders.nullable Mapping.scalarEncoder)))))
          ]
      )
  
  scalarDecoder =
    Decoders.composite
      (Just "public")
      "track_info"
      ( TrackInfo
          <$> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nullable (Decoders.array (Decoders.dimension Vector.replicateM (Decoders.element (Decoders.nullable Mapping.scalarDecoder)))))
      )
  
