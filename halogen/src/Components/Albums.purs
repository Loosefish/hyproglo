module Components.Albums where

import Hpg.Prelude

import Control.Parallel (parSequence)

import Data.Array as A
import Data.Array ((:))
import Data.String as S

import Halogen (Component, ComponentDSL, ComponentHTML)
import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (href, src, colSpan)

import Model (Artist(..), Album(..), Song(..), AppEffects)
import Mpd (fetchSongs, fetchAlbums)
import Util (toClass, fa, clickable, nbsp, addProp, stripNum, formatTime)


type State =
    { artist :: Artist
    , albums :: Array (Tuple Album (Array Song))
    , focus :: Maybe Album
    }


init :: Artist -> State
init a =
    { artist: a
    , albums: []
    , focus: Nothing
    }


data Query a = LoadAlbums a | Focus Album a

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = H.lifecycleComponent
    { render
    , eval
    , initializer: Just $ H.action LoadAlbums
    , finalizer: Nothing
    }
  where
    eval :: Query ~> ComponentDSL State Query (Aff (AppEffects eff))
    eval (LoadAlbums next) = do
        newAlbums <- H.fromAff <$> fetchAlbums =<< H.gets _.artist
        songs <- H.fromAff <$> parSequence $ map fetchSongs newAlbums
        let withSongs = A.zip newAlbums songs
        H.modify (_ { albums = withSongs })
        pure next

    eval (Focus album next) = do
        H.modify (_ { focus = Just album })
        pure next

    render :: State -> ComponentHTML Query
    render state =
        HH.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"]
            (header : links : HH.hr_ : map albumRow albums)
      where
        name = (\(Artist { name }) -> name) state.artist
        albums = state.albums
        header = HH.h4 [toClass "page-header"] [HH.text name]

        links = HH.ul [toClass "nav nav-pills"]
            (HH.li_ [HH.a [href "#/music"] [fa "arrow-left"]] : map albumLink albums)

        albumLink (Tuple album _) = HH.li [clickable] [HH.a_ $ albumTitle album]

        albumTitle (Album { title, date }) = [HH.text $ title <> nbsp, HH.small_ $ map HH.text ["(", S.take 4 date, ")"]]

        albumRow (Tuple album songs) =
            HH.div [toClass "row"]
                [ HH.div [toClass "col-xs-12"] [HH.h5_ [HH.span [clickable] $ albumTitle album, HH.text nbsp, addProp (clickable) $ fa "plus-circle"]]
                , HH.div [toClass "col-xs-12 col-sm-8"] $ [songTable songs]
                , HH.div [toClass "col-sm-4 col-xs-hidden"] [image $ A.head songs]
                ]

        image (Just (Song { file })) = HH.img [src $ "/cgi-bin/image?file=" <> file, toClass "img-responsive center-block"]
        image Nothing = HH.img []

        songTable songs = HH.table [toClass "table table-condensed table-hover"]
            [ HH.tbody_ $ A.concatMap songOrDisc songs
            , HH.tfoot_ [footer]
            ]
          where
            various = any (\(Song { artist }) -> artist /= name) songs
            multidisc = 1 < A.length (A.nub $ map (\(Song s) -> s.disc) songs)
            width = if various then 5 else 4

            songOrDisc s@(Song { track, disc }) | multidisc && maybe false ((==) "1") (stripNum track) =
                [HH.tr_ [HH.th [colSpan width] [fa "circle", HH.text $ nbsp <> (fromMaybe "" $ stripNum disc)]], songRow s]
            songOrDisc s = [songRow s]

            songRow (Song { title, track, time, artist, disc }) =
                HH.tr_ $ A.catMaybes
                    [ Just $ HH.td_ [HH.text $ fromMaybe "" $ stripNum track]
                    , if various then Just $ HH.td_ [HH.text artist] else Nothing
                    , Just $ HH.td [clickable] [HH.text title]
                    , Just $ HH.td_ [HH.text $ formatTime time]
                    , Just $ HH.td [clickable] [fa "plus-circle"]
                    ]

            footer = HH.tr_ $ 
                [ HH.td [colSpan $ width - 2] []
                , HH.td_ [HH.text $ formatTime $ sum $ map (\(Song s) -> s.time) songs]
                ]
