module Model where

import Hpg.Prelude


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
