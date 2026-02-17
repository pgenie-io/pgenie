module MySpace.MusicCatalogue.Statements.SelectVinylByCountry where

import MySpace.MusicCatalogue.Prelude
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Hasql.Mapping as Mapping
import qualified MySpace.MusicCatalogue.Types as Types

-- |
-- Parameters for the @select_vinyl_by_country@ query.
--
-- ==== SQL Template
--
-- > -- Find all vinyl albums recorded in a specific country
-- > select 
-- >   album.id,
-- >   album.name,
-- >   album.released,
-- >   album.format,
-- >   (album.recording).studio_name as studio,
-- >   (album.recording).city as city,
-- >   (album.recording).country as country,
-- >   (album.recording).recorded_date as recorded_date
-- > from album
-- > where 
-- >   album.format = 'Vinyl'
-- >   and (album.recording).country = $country
-- > order by (album.recording).recorded_date
--
-- ==== Source Path
--
-- > ./queries/select_vinyl_by_country.sql
--
newtype SelectVinylByCountry = SelectVinylByCountry
  { -- | Maps to @country@.
    country :: Maybe (Text)
  }
  deriving stock (Eq, Show)

-- | Result of the statement parameterised by 'SelectVinylByCountry'.
type SelectVinylByCountryResult = Vector.Vector SelectVinylByCountryResultRow

-- | Row of 'SelectVinylByCountryResult'.
data SelectVinylByCountryResultRow = SelectVinylByCountryResultRow
  { -- | Maps to @id@.
    id :: Int64,
    -- | Maps to @name@.
    name :: Text,
    -- | Maps to @released@.
    released :: Maybe (Day),
    -- | Maps to @format@.
    format :: Maybe (Types.AlbumFormat),
    -- | Maps to @studio@.
    studio :: Maybe (Text),
    -- | Maps to @city@.
    city :: Maybe (Text),
    -- | Maps to @country@.
    country :: Maybe (Text),
    -- | Maps to @recorded_date@.
    recordedDate :: Maybe (Day)
  }
  deriving stock (Show, Eq)


instance Mapping.IsStatement SelectVinylByCountry where
  type Result SelectVinylByCountry = SelectVinylByCountryResult

  statement = Statement.preparable sql encoder decoder
    where
      sql =
        "-- Find all vinyl albums recorded in a specific country\n\
        \select \n\
        \  album.id,\n\
        \  album.name,\n\
        \  album.released,\n\
        \  album.format,\n\
        \  (album.recording).studio_name as studio,\n\
        \  (album.recording).city as city,\n\
        \  (album.recording).country as country,\n\
        \  (album.recording).recorded_date as recorded_date\n\
        \from album\n\
        \where \n\
        \  album.format = 'Vinyl'\n\
        \  and (album.recording).country = $1\n\
        \order by (album.recording).recorded_date"

      encoder =
        mconcat
          [ (.country) >$< Encoders.param (Encoders.nullable (Mapping.scalarEncoder))
          ]

      decoder =
        Decoders.rowVector do
          id <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          name <- Decoders.column (Decoders.nonNullable (Mapping.scalarDecoder))
          released <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          format <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          studio <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          city <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          country <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          recordedDate <- Decoders.column (Decoders.nullable (Mapping.scalarDecoder))
          pure SelectVinylByCountryResultRow {..}

