module Model where

import Hpg.Prelude

import Halogen (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import DOM.HTML.Types (WINDOW)
import Control.Monad.Eff.Console (CONSOLE)


type State =
    { busy :: Boolean
    , albumArtists :: Array Artist
    , view :: View
    }


data View
    = AlbumArtists
    | Albums Artist (Array (Tuple Album (Array Song)))
    | Playlist


initialState :: State
initialState =
    { busy: false
    , albumArtists: []
    , view: AlbumArtists
    }


data Query a
    = FetchArtists a
    | ChangeView View a


newtype Artist = Artist { name :: String }

newtype Album = Album
    { artist :: Artist
    , title :: String
    , date :: String
    }

newtype Song = Song
    { disc :: Maybe String 
    , file :: String
    , time :: Int
    , title :: String
    , artist :: String
    , track :: Maybe String
    }


type AppEffects eff = HalogenEffects
    ( ajax :: AJAX
    , console :: CONSOLE
    , window :: WINDOW
    | eff)
