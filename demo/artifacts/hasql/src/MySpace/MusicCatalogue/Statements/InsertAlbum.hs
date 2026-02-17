module MySpace.MusicCatalogue.Statements.InsertAlbum where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @insert_album@ query.
--
-- ==== SQL Template
--
-- > insert into album (name, released)
-- > values ($name, $released)
-- > returning id
--
-- ==== Source Path
--
-- > ./queries/insert_album.sql
--
data InsertAlbum = InsertAlbum
  { -- | Maps to @name@.
    name :: Text,
    -- | Maps to @released@.
    released :: Maybe (Day)
  }
  deriving stock (Eq, Show)

-- | Result of the statement parameterised by 'InsertAlbum'.
type InsertAlbumResult = InsertAlbumResultRow

-- | Row of 'InsertAlbumResult'.
newtype InsertAlbumResultRow = InsertAlbumResultRow
  { -- | Maps to @id@.
    id :: Int64
  }
  deriving stock (Show, Eq)


instance Mapping.IsStatement InsertAlbum where
  type Result InsertAlbum = InsertAlbumResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "insert into album (name, released)\n\
        \values ($1, $2)\n\
        \returning id"

      encoder =
        mconcat
          [ (.name) >$< Encoders.param (Encoders.nonNullable (Mapping.scalarEncoder)),
            (.released) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder))
          ]

      decoder =
        Decoders.singleRow do
          id <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          pure InsertAlbumResultRow {..}

