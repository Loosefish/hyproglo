module Model where

import Hpg.Prelude

import Halogen (HalogenEffects, Component, ComponentDSL)
import Network.HTTP.Affjax (AJAX)
import DOM.HTML.Types (WINDOW)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)


type AppEffects eff = HalogenEffects
    ( ajax :: AJAX
    , console :: CONSOLE
    , window :: WINDOW
    , random :: RANDOM
    | eff)

type AppChild s q =
  forall eff. Unit -> { component :: Component s q (Aff (AppEffects eff)), initialState :: s }

type AppUpdate q s = forall eff. q ~> ComponentDSL s q (Aff (AppEffects eff))

type AppUi s q = forall eff. Component s q (Aff (AppEffects eff))


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
-- volume: -1
-- consume: 0
-- playlist: 2
-- playlistlength: 31
-- mixrampdb: 0.000000
-- state: pause
-- song: 3
-- songid: 4
-- !time: 189:272
-- !elapsed: 189.289
-- !bitrate: 0
-- !audio: 44100:24:2
-- nextsong: 4
-- nextsongid: 5
