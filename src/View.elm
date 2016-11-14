module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import String

import Model exposing (Model, Msg(..), Perspective(..))
import Mpd exposing (getAlbumArtists)


view : Model -> Html Msg
view model =
    let
        sidebar =
            [ h4 [class "page-header"] [text "HyProGlo"]
            , navs
            , div [] <| filterMaybe [Just controls, status]
            ]

        main =
            case model.perspective of
                Music Nothing ->
                    viewAlbumArtists model

                Music (Just artist) ->
                    [h4 [class "page-header"] [text artist.name]]

                Playlist -> 
                    [h4 [class "page-header"] [text "Playlist"]]

        navs = 
            ul [class "nav nav-sidebar"]
                [ li (active (Music Nothing))
                    [a [href "#/music", onClick <| ChangePerspective (Music Nothing)]
                        [fa "database", text " Music "]]
                , li (active Playlist)
                    [a [href "#/playlist", onClick <| ChangePerspective Playlist]
                        [fa "list", text " Playlist ", span [class "badge pull-right"] [text "7"]]]

                ]

        active p =
            if p == model.perspective then
                [class "active"]
            else
                []

        controls =
            div []
                [ div [class "btn-group btn-group-sm btn-group-justified"]
                    [btnFa "play"]
                , div [class "btn-group btn-group-sm btn-group-justified"]
                    [btnFa "fast-backward", btnFa "stop", btnFa "fast-forward"]
                , div [class "btn-group btn-group-sm btn-group-justified"]
                    [btnFa "random", btnFa "repeat", btn <| text "1"]
                ]

        status =
            Just <| div []
                [ div [class "progress"]
                    [div [class "progress-bar", style [("width", "16%")]] [text "0:16 / 5:07"]]
                , img [class "img-responsive hidden-xs center-block", src ""] []
                , a [href "#"]
                    [ h5 [class "text-center"] [text "Computerwelt"]
                    , h6 [class "text-center"] [text "Kraftwerk"]
                    , h6 [class "text-center"] [text "Computerwelt", small [] [text " (1981)"]]
                    ]
                ]
    in
        div [class "row"]
            [ div [class "col-xs-12 col-sm-3 col-md-2 sidebar"] sidebar
            , div [class "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] main
            ]


fa : String -> Html Msg
fa icon =
    span [class <| "fa fa-" ++ icon] []


btn : Html Msg -> Html Msg
btn content =
    a [type' "button", class "btn btn-default"] [content]


btnFa : String -> Html Msg
btnFa = fa >> btn


filterMaybe : List (Maybe a) -> List a
filterMaybe = List.filterMap (\x -> x)


viewAlbumArtists : Model -> List (Html Msg)
viewAlbumArtists model = 
    let
        header =
            h4 [class "page-header"] [text "Album Artists"]

        grouped =
            List.map .name model.artists
            |> groupBy (String.left 1 >> String.toUpper)

        letter (l, names) =
            span []
                [ span [] [text l]
                , ul [class "nav nav-pills"] <| List.map artistPill names
                ]

        artistPill name =
            let
                link =
                    href <| "#/music/" ++ name

                action =
                    onClick <| ChangePerspective (Music (Just <| Model.Artist name))
            in
                li [] [a [link, action] [text name]]
    in
        header :: (List.map letter <| Dict.toList grouped)


groupBy : (comparable -> comparable') -> List comparable -> Dict comparable' (List comparable)
groupBy key =
    let
        groupBy' d things =
            case things of
                x :: xs ->
                    groupBy' (Dict.update (key x) (append x) d) xs

                [] ->
                    d

        append y ys =
            Just <| y :: Maybe.withDefault [] ys
    in
        Dict.map (\_ l -> List.sort l) << groupBy' Dict.empty
