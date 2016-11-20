module View (render) where

import Hpg.Prelude

import Data.Array ((:))
import Data.Array as A
import Data.Char (fromCharCode)
import Data.String as S

import Halogen (ComponentHTML)
import Halogen.HTML.Core (className, HTML(Element))
{-- import Halogen.HTML.Events.Indexed as HE --}
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (classes, href, IProp(..), I, src, colSpan)

import Model


render :: State -> ComponentHTML Query
render st =
    H.div [toClass "container-fluid"] 
    [ H.div [toClass "row"]
        [ H.div [toClass "col-xs-12 col-sm-3 col-md-2 sidebar"] left
        , H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] right
        ]
    ]
  where
    left =
        [ H.h4 [toClass "page-header"]
            [ H.text $ "HyProGlo" <> nbsp
            , if st.busy then fa "circle-o-notch fa-spin fa-fw" else H.text ""
            ]
        , nav
        , H.div_ []
        ]

    nav =
        H.ul [toClass "nav nav-sidebar"]
        [ music
        , playlist
        ]

    music =
        case st.view of
             Playlist -> (\x -> x)
             _ -> addProp (toClass "active")
        $ viewLiA "#/music"
            [ fa "database fa-fw"
            , H.text $ nbsp <> "Music" <> artist
            ]
      where
        artist = case st.view of
            Albums (Artist { name }) _ -> nbsp <> "/" <> nbsp <> name
            _ -> ""

    playlist =
        case st.view of
             Playlist -> addProp (toClass "active")
             _ -> id
        $ viewLiA "#/playlist"
            [fa "list fa-fw"
            , H.text $ nbsp <> "Playlist"
            , H.span [toClass "badge pull-right"] [H.text "0"]
            ]

    right = case st.view of
        AlbumArtists -> renderAlbumArtists st.albumArtists
        Albums artist albums -> renderAlbums artist albums
        Playlist -> [H.h4 [toClass "page-header"] [H.text "Playlist"]]


renderAlbumArtists :: forall a. Array Artist -> Array (HTML a (Query Unit))
renderAlbumArtists artists = 
    (H.h4 [toClass "page-header"] [H.text "Album Artists"])
    : (map (artistGroup <<< A.fromFoldable) $ A.groupBy (\a b -> letter a == letter b) artists)
  where
    artistLink a@(Artist { name }) = viewLiA ("#/music/" <> name) [H.text name]

    artistGroup group = H.span_ $ A.catMaybes
        [ H.text <<< letter <$> A.head group
        , Just $ H.ul [toClass "nav nav-pills"] $ map artistLink group
        ]

    letter (Artist { name }) = S.toUpper $ S.take 1 name


renderAlbums :: forall a. Artist -> Array (Tuple Album (Array Song)) -> Array (HTML a (Query Unit))
renderAlbums (Artist { name }) albums = header : links : H.hr_ : map albumRow albums
  where
    header = H.h4 [toClass "page-header"] [H.text name]

    links = H.ul [toClass "nav nav-pills"]
        (H.li_ [H.a [href "#/music"] [fa "arrow-left"]] : map albumLink albums)

    albumLink (Tuple album _) = H.li [clickable] [H.a_ $ albumTitle album]

    albumTitle (Album { title, date }) = [H.text $ title <> nbsp, H.small_ $ map H.text ["(", S.take 4 date, ")"]]

    albumRow (Tuple album songs) =
        H.div [toClass "row"]
            [ H.div [toClass "col-xs-12"] [H.h5_ [H.span [clickable] $ albumTitle album, H.text nbsp, addProp (clickable) $ fa "plus-circle"]]
            , H.div [toClass "col-xs-12 col-sm-8"] $ [songTable songs]
            , H.div [toClass "col-sm-4 col-xs-hidden"] [image $ A.head songs]
            ]

    image (Just (Song { file })) = H.img [src $ "/cgi-bin/image?file=" <> file, toClass "img-responsive center-block"]
    image Nothing = H.img []

    songTable songs = H.table [toClass "table table-condensed table-hover"]
        [ H.tbody_ $ A.concatMap songOrDisc songs
        , H.tfoot_ [footer]
        ]
      where
        various = any (\(Song { artist }) -> artist /= name) songs
        multidisc = 1 < A.length (A.nub $ map (\(Song s) -> s.disc) songs)
        width = if various then 5 else 4

        songOrDisc s@(Song { track, disc }) | multidisc && maybe false ((==) "1") (stripNum track) =
            [H.tr_ [H.th [colSpan width] [fa "circle", H.text $ nbsp <> (fromMaybe "" $ stripNum disc)]], songRow s]
        songOrDisc s = [songRow s]

        songRow (Song { title, track, time, artist, disc }) =
            H.tr_ $ A.catMaybes
                [ Just $ H.td_ [H.text $ fromMaybe "" $ stripNum track]
                , if various then Just $ H.td_ [H.text artist] else Nothing
                , Just $ H.td [clickable] [H.text title]
                , Just $ H.td_ [H.text $ formatTime time]
                , Just $ H.td [clickable] [fa "plus-circle"]
                ]

        footer = H.tr_ $ 
            [ H.td [colSpan $ width - 2] []
            , H.td_ [H.text $ formatTime $ sum $ map (\(Song s) -> s.time) songs]
            ]
            


toClass :: forall r i. String -> IProp (class :: I | r) i
toClass s = classes $ map className $ S.split (Pattern " ") s


addProp :: forall r p i. IProp r i -> HTML p i -> HTML p i
addProp (IProp p) (Element n t ps cs) = Element n t (p : ps) cs
addProp _ element = element


fa :: forall p i. String -> HTML p i
fa name = H.span [toClass $ "fa fa-" <> name] []


nbsp :: String
nbsp = S.singleton $ fromCharCode 160


viewLiA :: forall a. String -> Array (HTML a (Query Unit)) -> HTML a (Query Unit)
viewLiA h c = H.li_ [H.a [href h] c]


stripNum :: Maybe String -> Maybe String
stripNum track = S.takeWhile ((/=) '/') <$> track


formatTime :: Int -> String
formatTime total = show minutes <> ":" <> pad seconds
  where
    minutes = total / 60
    seconds = total `mod` 60
    pad s = (if s < 10 then "0" else "") <> show s


clickable  = toClass "clickable"
