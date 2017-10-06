module Model where

import Hpg.Prelude

import Network.HTTP.Affjax (AJAX)
import DOM.HTML.Types (WINDOW)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array as A
import Data.String as S


type AppEffects eff = 
    ( ajax :: AJAX
    , console :: CONSOLE
    , window :: WINDOW
    , random :: RANDOM
    | eff)


-- Artist

newtype Artist = Artist { name :: String }
derive instance eqArtist :: Eq Artist


artistName :: Artist -> String
artistName (Artist a) = a.name


-- Album

newtype Album = Album
    { artist :: Artist
    , title :: String
    , date :: String
    }
derive instance eqAlbum :: Eq Album
    
albumId :: Album -> String
albumId (Album a) = a.date <> "|" <> a.title

albumUrl :: Album -> String
albumUrl (Album a) = S.joinWith "/" ["#", "music", artistName a.artist, a.date, a.title]

albumTitle :: Album -> String
albumTitle (Album a) = a.title

-- Song

newtype Song = Song
    { album :: Maybe Album
    , artist :: String
    , disc :: Maybe String 
    , file :: String
    , time :: Int
    , title :: String
    , track :: Maybe String
    }
derive instance eqSong :: Eq Song

songAlbum :: Song -> Maybe Album
songAlbum (Song s) = s.album

songArtist :: Song -> String
songArtist (Song s) = s.artist

songDisc :: Song -> Maybe String
songDisc (Song s) = s.disc

songsByAlbum :: Array Song -> Array (Tuple Album (Array Song))
songsByAlbum songs = map (\a -> Tuple a $ filter a ) albums
  where
    albums = A.nub $ A.mapMaybe (\(Song { album }) -> album) songs
    filter album =
        A.filter (\(Song s) -> maybe false ((==) album) s.album) songs

-- Status

newtype Status = Status
    { playState :: PlayState
    , playlist :: Int
    , playlistLength :: Int
    , playlistSong :: Maybe Int
    , random :: Boolean
    , repeat :: Boolean
    , single :: Boolean
    , time :: Maybe (Tuple Int Int)
    }

statusPlaylist :: Status -> Int
statusPlaylist (Status s) = s.playlist

statusPlaylistSong :: Status -> Maybe Int
statusPlaylistSong (Status s) = s.playlistSong

statusPlayState :: Status -> PlayState
statusPlayState (Status s) = s.playState

data PlayState = Playing | Stopped | Paused
derive instance eqPlayState :: Eq PlayState
