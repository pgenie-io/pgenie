module MySpace.MusicCatalogue.Statements.UpdateAlbumReleased where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @update_album_released@ query.
--
-- ==== SQL Template
--
-- > update album
-- > set released = $released
-- > where id = $id
-- > 
--
-- ==== Source Path
--
-- > ./queries/update_album_released.sql
--
data UpdateAlbumReleased = UpdateAlbumReleased
  { -- | Maps to @released@.
    released :: Maybe (Day),
    -- | Maps to @id@.
    id :: Maybe (Int64)
  }
  deriving stock (Eq, Show)

type UpdateAlbumReleasedResult = Int


instance Mapping.IsStatement UpdateAlbumReleased where
  type Result UpdateAlbumReleased = UpdateAlbumReleasedResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "update album\n\
\set released = $1\n\
\where id = $2\n\
\"

      encoder =
        mconcat
          [ (.released) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder)),
            (.id) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder))
          ]

      decoder =
        fromIntegral <$> Decoders.rowsAffected

