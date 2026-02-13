module MySpace.MusicCatalogue.Statements.UpdateAlbumReleasedReturning where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @update_album_released_returning@ query.
--
-- ==== SQL Template
--
-- > update album
-- > set released = $released
-- > where id = $id
-- > returning *
-- > 
--
-- ==== Source Path
--
-- > ./queries/update_album_released_returning.sql
--
data UpdateAlbumReleasedReturning = UpdateAlbumReleasedReturning
  { -- | Maps to @released@.
    released :: Maybe (Day),
    -- | Maps to @id@.
    id :: Maybe (Int64)
  }
  deriving stock (Eq, Show)

-- | Result of the statement parameterised by 'UpdateAlbumReleasedReturning'.
type UpdateAlbumReleasedReturningResult = Vector.Vector UpdateAlbumReleasedReturningResultRow

-- | Row of 'UpdateAlbumReleasedReturningResult'.
data UpdateAlbumReleasedReturningResultRow = UpdateAlbumReleasedReturningResultRow
  { -- | Maps to @id@.
    id :: Int64,
    -- | Maps to @name@.
    name :: Text,
    -- | Maps to @released@.
    released :: Maybe (Day)
  }
  deriving stock (Show, Eq)


instance Mapping.IsStatement UpdateAlbumReleasedReturning where
  type Result UpdateAlbumReleasedReturning = UpdateAlbumReleasedReturningResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "update album\n\
\set released = $1\n\
\where id = $2\n\
\returning *\n\
\"

      encoder =
        mconcat
          [ (.released) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder)),
            (.id) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          released <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          pure UpdateAlbumReleasedReturningResultRow {..}

