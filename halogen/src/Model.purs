module Model where

import Hpg.Prelude

import Halogen (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import DOM.HTML.Types (WINDOW)
import Control.Monad.Eff.Console (CONSOLE)


type AppEffects eff = HalogenEffects
    ( ajax :: AJAX
    , console :: CONSOLE
    , window :: WINDOW
    | eff)


-- Artist

newtype Artist = Artist { name :: String }
derive instance eqArtist :: Eq Artist
artistName (Artist a) = a.name


-- Album

newtype Album = Album
    { artist :: Artist
    , title :: String
    , date :: String
    }
derive instance eqAlbum :: Eq Album

albumArtist (Album a) = a.artist
albumDate (Album a) = a.date
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

songAlbum (Song s) = s.album
songArtist (Song s) = s.artist
songDisc (Song s) = s.disc
songFile (Song s) = s.file
songTime (Song s) = s.time
songTitle (Song s) = s.title
songTrack (Song s) = s.track


-- Status

newtype Status = Status
    { repeat :: Boolean
    , random :: Boolean
    , single :: Boolean
    }
-- volume: -1
-- consume: 0
-- playlist: 2
-- playlistlength: 31
-- mixrampdb: 0.000000
-- state: pause
-- song: 3
-- songid: 4
-- time: 189:272
-- elapsed: 189.289
-- bitrate: 0
-- audio: 44100:24:2
-- nextsong: 4
-- nextsongid: 5
