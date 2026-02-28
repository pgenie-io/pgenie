module MySpace.MusicCatalogue.Types.DiscInfo where

import Data.Aeson qualified as Aeson
import Data.Vector qualified as Vector
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Mapping qualified as Mapping
import MySpace.MusicCatalogue.Prelude

-- |
-- Representation of the @disc_info@ user-declared PostgreSQL record type.
data DiscInfo = DiscInfo
  { -- | Maps to @name@.
    name :: Maybe (Text),
    -- | Maps to @recording@.
    recording :: Maybe (Types.RecordingInfo)
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
