module Main exposing (main)

import Html.App exposing (program)
import Process
import Task exposing (Task)
import Time exposing (Time)

import Model exposing (Model, Msg(..))
import Mpd exposing (getAlbumArtists)
import View exposing (view)


main =
    program
        { init = (Model.default, getAlbumArtists)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Fetch cmd ->
            (model, cmd)

        GotAlbumArtists (Ok artists) ->
            ({ model | artists = artists }, Cmd.none)

        GotAlbumArtists (Err err) ->
            (model, delay (5 * Time.second) (Fetch getAlbumArtists))

        ChangePerspective p ->
            ({ model | perspective = p }, Cmd.none)

        _ -> 
            (model, Cmd.none)


delay : Time -> Msg -> Cmd Msg
delay t msg =
    Task.perform (\_ -> NoOp) (\_ -> msg) <| Process.sleep t
