module MySpace.MusicCatalogue.Statements.SelectAlbumByReleased where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @select_album_by_released@ query.
--
-- ==== SQL Template
--
-- > select id, name, released, format
-- > from album
-- > where released = $released
--
-- ==== Source Path
--
-- > ./queries/select_album_by_released.sql
--
newtype SelectAlbumByReleased = SelectAlbumByReleased
  { -- | Maps to @released@.
    released :: Maybe (Day)
  }
  deriving stock (Eq, Show)

-- | Result of the statement parameterised by 'SelectAlbumByReleased'.
type SelectAlbumByReleasedResult = Vector.Vector SelectAlbumByReleasedResultRow

-- | Row of 'SelectAlbumByReleasedResult'.
data SelectAlbumByReleasedResultRow = SelectAlbumByReleasedResultRow
  { -- | Maps to @id@.
    id :: Int64,
    -- | Maps to @name@.
    name :: Text,
    -- | Maps to @released@.
    released :: Maybe (Day),
    -- | Maps to @format@.
    format :: Maybe (Types.AlbumFormat)
  }
  deriving stock (Show, Eq)


instance Mapping.IsStatement SelectAlbumByReleased where
  type Result SelectAlbumByReleased = SelectAlbumByReleasedResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "select id, name, released, format\n\
        \from album\n\
        \where released = $1"

      encoder =
        mconcat
          [ (.released) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          released <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          format <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          pure SelectAlbumByReleasedResultRow {..}
