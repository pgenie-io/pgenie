-- | Mappings to all queries in the project.
-- 
-- Hasql statements are provided by the 'Hasql.Mapping.IsStatement' typeclass instances indexed by the statement parameter type.
-- 
module MySpace.MusicCatalogue.Statements 
  ( -- ** InsertAlbum
    module MySpace.MusicCatalogue.Statements.InsertAlbum,
    -- ** SelectAlbumByArtist
    module MySpace.MusicCatalogue.Statements.SelectAlbumByArtist,
    -- ** SelectAlbumByFormat
    module MySpace.MusicCatalogue.Statements.SelectAlbumByFormat,
    -- ** SelectAlbumsWithRecordingInfo
    module MySpace.MusicCatalogue.Statements.SelectAlbumsWithRecordingInfo,
    -- ** SelectGenreByArtist
    module MySpace.MusicCatalogue.Statements.SelectGenreByArtist,
    -- ** SelectVinylByCountry
    module MySpace.MusicCatalogue.Statements.SelectVinylByCountry,
    -- ** UpdateAlbumRecording
    module MySpace.MusicCatalogue.Statements.UpdateAlbumRecording,
    -- ** UpdateAlbumReleased
    module MySpace.MusicCatalogue.Statements.UpdateAlbumReleased,
    -- ** UpdateAlbumReleasedReturning
    module MySpace.MusicCatalogue.Statements.UpdateAlbumReleasedReturning,
  )
where

import MySpace.MusicCatalogue.Statements.InsertAlbum
import MySpace.MusicCatalogue.Statements.SelectAlbumByArtist
import MySpace.MusicCatalogue.Statements.SelectAlbumByFormat
import MySpace.MusicCatalogue.Statements.SelectAlbumsWithRecordingInfo
import MySpace.MusicCatalogue.Statements.SelectGenreByArtist
import MySpace.MusicCatalogue.Statements.SelectVinylByCountry
import MySpace.MusicCatalogue.Statements.UpdateAlbumRecording
import MySpace.MusicCatalogue.Statements.UpdateAlbumReleased
import MySpace.MusicCatalogue.Statements.UpdateAlbumReleasedReturning
