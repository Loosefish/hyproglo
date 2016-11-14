module Model exposing (..)

import Http exposing (Error)


type Msg = Fetch (Cmd Msg)
    | GotAlbumArtists (Result Error (List Artist))
    | ChangePerspective Perspective
    | NoOp


type alias Model =
    { perspective : Perspective
    , artists : List Artist
    }


type Perspective =
    Music (Maybe Artist)
    | Playlist

type alias Artist = {name : String}


default : Model
default =
    Model (Music Nothing )[]
