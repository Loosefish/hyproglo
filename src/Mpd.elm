module Mpd exposing(getAlbumArtists)

import Json.Decode as Json exposing ((:=))
import Http
import Dict exposing (Dict)
import List.Extra
import Regex
import String
import Task exposing (Task)

import Model exposing (Artist, Msg(..))


type alias MpdAnswer =
    { status : Maybe String
    , error : Maybe String
    , result : Maybe String
    }


-- Artist

getAlbumArtists : Cmd Msg
getAlbumArtists =
    Task.perform (GotAlbumArtists << Err) parseArtists
    <| sendMpd "list AlbumArtist"


parseArtists : String -> Msg
parseArtists =
    parseMpd >> group >> List.filterMap toArtist >> Ok >> GotAlbumArtists


toArtist : Dict String String -> Maybe Artist
toArtist pairs =
    Maybe.map Artist
        <| Maybe.oneOf
            [ Dict.get "AlbumArtist" pairs
            , Dict.get "Artist" pairs
            ]


-- Album


sendMpd : String -> Task Http.Error String
sendMpd query =
    let
        post : Task Http.Error MpdAnswer
        post =
            Http.post mpdDecoder "/cgi-bin/mpd" (Http.string query)

        validate : MpdAnswer -> Task Http.Error String
        validate mpd =
            case (mpd.error, mpd.status, mpd.result) of
                (Just error, _, _) ->
                    fail error

                (Nothing, Nothing, _) ->
                    fail "empty status"

                (_, _, Nothing) ->
                    fail "empty result"

                (Nothing, Just status, Just result) ->
                    if String.startsWith "OK MPD" status then
                        Task.succeed result
                    else
                        fail status

        fail : String -> Task Http.Error a
        fail =
            Task.fail << Http.UnexpectedPayload
    in
        post `Task.andThen` validate


mpdDecoder : Json.Decoder MpdAnswer
mpdDecoder =
    Json.object3 MpdAnswer
        (Json.maybe ("status" := Json.string))
        (Json.maybe ("error" := Json.string))
        (Json.maybe ("result" := Json.string))


parseMpd : String -> List (String, String)
parseMpd raw =
    let
        toTuple : List String -> Maybe (String, String)
        toTuple xs =
            case xs of
                a :: b :: _ -> Just (a, b)
                _ -> Nothing

        split : String -> Maybe (String, String)
        split =
            toTuple << Regex.split (Regex.AtMost 1) (Regex.regex ": ")
    in
        if String.endsWith "OK\n" raw then
            String.lines raw
            |> List.Extra.init
            |> Maybe.withDefault []
            |> List.filterMap split
        else
            []


group : List (String, String) -> List (Dict String String)
group =
    let
        group' : List (String, String) -> List (List (String, String))
        group' pairs =
            case pairs of
                (key, val) :: ps ->
                    let
                        (here, rest) = List.Extra.break (\(k, _) -> k == key) ps
                    in
                        ((key, val) :: here) :: group' rest

                _ -> []
    in
        List.map Dict.fromList << group'



