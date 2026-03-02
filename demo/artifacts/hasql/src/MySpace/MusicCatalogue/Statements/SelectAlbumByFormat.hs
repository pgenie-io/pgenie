module MySpace.MusicCatalogue.Statements.SelectAlbumByFormat where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @select_album_by_format@ query.
--
-- ==== SQL Template
--
-- > select 
-- >   id,
-- >   name,
-- >   released,
-- >   format,
-- >   recording
-- > from album
-- > where format = $format
--
-- ==== Source Path
--
-- > ./queries/select_album_by_format.sql
--
newtype SelectAlbumByFormat = SelectAlbumByFormat
  { -- | Maps to @format@.
    format :: Maybe (Types.AlbumFormat)
  }
  deriving stock (Eq, Show)

-- | Result of the statement parameterised by 'SelectAlbumByFormat'.
type SelectAlbumByFormatResult = Vector.Vector SelectAlbumByFormatResultRow

-- | Row of 'SelectAlbumByFormatResult'.
data SelectAlbumByFormatResultRow = SelectAlbumByFormatResultRow
  { -- | Maps to @id@.
    id :: Int64,
    -- | Maps to @name@.
    name :: Text,
    -- | Maps to @released@.
    released :: Maybe (Day),
    -- | Maps to @format@.
    format :: Maybe (Types.AlbumFormat),
    -- | Maps to @recording@.
    recording :: Maybe (Types.RecordingInfo)
  }
  deriving stock (Show, Eq)

instance Mapping.IsStatement SelectAlbumByFormat where
  type Result SelectAlbumByFormat = SelectAlbumByFormatResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "select \n\
        \  id,\n\
        \  name,\n\
        \  released,\n\
        \  format,\n\
        \  recording\n\
        \from album\n\
        \where format = $1"

      encoder =
        mconcat
          [ (.format) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          released <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          format <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          recording <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          pure SelectAlbumByFormatResultRow {..}

