-- | Mappings to all queries in the project.
--
-- Hasql statements are provided by the 'Hasql.Mapping.IsStatement' typeclass instances indexed by the statement parameter type.
module MySpace.MusicCatalogue.Statements
  ( -- ** InsertAlbum
    module MySpace.MusicCatalogue.Statements.InsertAlbum,

    -- ** SelectAlbumByFormat
    module MySpace.MusicCatalogue.Statements.SelectAlbumByFormat,

    -- ** SelectAlbumByName
    module MySpace.MusicCatalogue.Statements.SelectAlbumByName,

    -- ** SelectAlbumWithTracks
    module MySpace.MusicCatalogue.Statements.SelectAlbumWithTracks,

    -- ** SelectGenreByArtist
    module MySpace.MusicCatalogue.Statements.SelectGenreByArtist,

    -- ** UpdateAlbumRecordingReturning
    module MySpace.MusicCatalogue.Statements.UpdateAlbumRecordingReturning,

    -- ** UpdateAlbumReleased
    module MySpace.MusicCatalogue.Statements.UpdateAlbumReleased,
  )
where

import MySpace.MusicCatalogue.Statements.InsertAlbum
import MySpace.MusicCatalogue.Statements.SelectAlbumByFormat
import MySpace.MusicCatalogue.Statements.SelectAlbumByName
import MySpace.MusicCatalogue.Statements.SelectAlbumWithTracks
import MySpace.MusicCatalogue.Statements.SelectGenreByArtist
import MySpace.MusicCatalogue.Statements.UpdateAlbumRecordingReturning
import MySpace.MusicCatalogue.Statements.UpdateAlbumReleased
