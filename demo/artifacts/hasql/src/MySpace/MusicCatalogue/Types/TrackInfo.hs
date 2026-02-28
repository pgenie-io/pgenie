module MySpace.MusicCatalogue.Types.TrackInfo where

import Data.Aeson qualified as Aeson
import Data.Vector qualified as Vector
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Mapping qualified as Mapping
import MySpace.MusicCatalogue.Prelude

-- |
-- Representation of the @track_info@ user-declared PostgreSQL record type.
data TrackInfo = TrackInfo
  { -- | Maps to @title@.
    title :: Maybe (Text),
    -- | Maps to @duration_seconds@.
    durationSeconds :: Maybe (Int32),
    -- | Maps to @tags@.
    tags :: Maybe (Vector (Text))
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
            (.tags) >$< Encoders.field (Encoders.nullable (Encoders.array (Encoders.dimension Vector.foldl' (Encoders.element (Encoders.nonNullable Mapping.scalarEncoder)))))
          ]
      )

  scalarDecoder =
    Decoders.composite
      (Just "public")
      "track_info"
      ( TrackInfo
          <$> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nullable (Mapping.scalarDecoder))
          <*> Decoders.field (Decoders.nullable (Decoders.array (Decoders.dimension Vector.replicateM (Decoders.element (Decoders.nonNullable Mapping.scalarDecoder)))))
      )
