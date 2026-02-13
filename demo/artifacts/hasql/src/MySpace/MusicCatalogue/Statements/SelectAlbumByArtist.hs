module MySpace.MusicCatalogue.Statements.SelectAlbumByArtist where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @select_album_by_artist@ query.
--
-- ==== SQL Template
--
-- > select album.*
-- > from album
-- > left join album_artist on album_artist.album = album.id
-- > where artist = $artist
-- > 
--
-- ==== Source Path
--
-- > ./queries/select_album_by_artist.sql
--
newtype SelectAlbumByArtist = SelectAlbumByArtist
  { -- | Maps to @artist@.
    artist :: Maybe (Int32)
  }
  deriving stock (Eq, Show)

-- | Result of the statement parameterised by 'SelectAlbumByArtist'.
type SelectAlbumByArtistResult = Vector.Vector SelectAlbumByArtistResultRow

-- | Row of 'SelectAlbumByArtistResult'.
data SelectAlbumByArtistResultRow = SelectAlbumByArtistResultRow
  { -- | Maps to @id@.
    id :: Int64,
    -- | Maps to @name@.
    name :: Text,
    -- | Maps to @released@.
    released :: Maybe (Day)
  }
  deriving stock (Show, Eq)


instance Mapping.IsStatement SelectAlbumByArtist where
  type Result SelectAlbumByArtist = SelectAlbumByArtistResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "select album.*\n\
\from album\n\
\left join album_artist on album_artist.album = album.id\n\
\where artist = $1\n\
\"

      encoder =
        mconcat
          [ (.artist) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          released <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          pure SelectAlbumByArtistResultRow {..}

