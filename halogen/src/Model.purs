module Model where

import Hpg.Prelude

import Halogen (HalogenEffects, Component, ComponentDSL)
import Network.HTTP.Affjax (AJAX)
import DOM.HTML.Types (WINDOW)
import Control.Monad.Eff.Console (CONSOLE)


type AppEffects eff = HalogenEffects
    ( ajax :: AJAX
    , console :: CONSOLE
    , window :: WINDOW
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


-- Status

newtype Status = Status
    { repeat :: Boolean
    , random :: Boolean
    , single :: Boolean
    , playState :: PlayState
    , time :: Maybe (Tuple Int Int)
    , playlistLength :: Int
    }

data PlayState = Playing | Stopped | Paused
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
