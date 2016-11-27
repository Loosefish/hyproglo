module Components.Albums where

import Hpg.Prelude

import Control.Parallel (parSequence)

import Data.Array as A
import Data.Array ((:))
import Data.String as S

import DOM.HTML.Types (WINDOW)

import Halogen (Component, ComponentDSL, ComponentHTML)
import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (src, colSpan, id_)

import Model (Artist(..), Album(..), Song(..), AppEffects, albumId, songAlbum)
import Mpd (fetchSongs, fetchAlbums', clear, addAlbum, addSong, play)
import Util (toClass, fa, clickable, nbsp, addProp, stripNum, formatTime, onClickDo)

foreign import scrollToId :: forall eff. String -> Eff (window :: WINDOW | eff) Unit


type State =
    { artist :: Artist
    , albums :: Array (Tuple Album (Array Song))
    , busy :: Boolean
    , currentSong :: Maybe Song
    }


init :: Artist -> State
init a =
    { artist: a
    , albums: []
    , busy: false
    , currentSong: Nothing
    }


data Query a
    = LoadAlbums a
    | Focus Album a
    | PlayAlbum Album Int a
    | AddSong Song a
    | SetArtist Artist a
    | SetCurrentSong (Maybe Song) a

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
        H.modify (_ { busy = true })
        withSongs <- H.fromAff <$> fetchAlbums' =<< H.gets _.artist
        H.modify (_ { albums = withSongs, busy = false })
        pure next

    eval (Focus album next) = do
        H.fromEff $ scrollToId $ albumId album
        pure next

    eval (PlayAlbum album i next) = do
        H.fromAff $ clear
        H.fromAff $ addAlbum album
        H.fromAff $ play i
        pure next

    eval (AddSong song next) = do
        H.fromAff $ addSong song
        pure next

    eval (SetArtist artist next) = do
      H.modify (_ {artist = artist})
      eval (LoadAlbums next)

    eval (SetCurrentSong song next) = do
      H.modify (_ {currentSong = song})
      pure next

    render :: State -> ComponentHTML Query
    render state =
      HH.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"]
      [ header
      , HH.span
        (if state.busy then [toClass "hidden"] else [])
        (links : HH.hr_ : map albumRow albums)
      ]
      where
        name = (\(Artist { name }) -> name) state.artist
        albums = state.albums
        header = HH.h4 [toClass "page-header clickable"] $ 
            (HH.text $ name <> nbsp) : if state.busy then [fa "circle-o-notch fa-spin"] else []

        links = HH.ul [toClass $ "nav nav-pills"] $ map albumLink albums

        albumLink (Tuple album _) = HH.li
            [ toClass $ "clickable" <> maybe "" (\s -> if songAlbum s == Just album then " active" else "") state.currentSong
            , onClickDo $ Focus album
            ]
            [HH.a_ $ albumTitle album]

        albumTitle (Album { title, date }) = [HH.text $ title <> nbsp, HH.small_ $ map HH.text ["(", S.take 4 date, ")"]]

        albumRow (Tuple album songs) =
            HH.div [id_ $ albumId album,toClass "row"]
                [ HH.div [toClass "col-xs-12"] [HH.h5_ [HH.span [clickable, onClickDo $ PlayAlbum album 0] $ albumTitle album, HH.text nbsp, addProp (clickable) $ fa "plus-circle"]]
                , HH.div [toClass "col-xs-12 col-sm-8"] $ [songTable]
                , HH.div [toClass "col-sm-4 col-xs-hidden"] [image $ A.head songs]
                ]
          where
            image Nothing = HH.img []
            image (Just (Song { file })) =
                HH.img [src $ "/image/" <> file, toClass "img-responsive center-block"]

            songTable = HH.table [toClass "table table-condensed table-hover"]
                [ HH.tbody_ $ A.concat $ A.mapWithIndex songOrDisc songs
                , HH.tfoot_ [footer]
                ]
              where
                various = any (\(Song { artist }) -> artist /= name) songs
                multidisc = 1 < A.length (A.nub $ map (\(Song s) -> s.disc) songs)
                width = if various then 5 else 4

                songOrDisc i s@(Song { track, disc }) | multidisc && maybe false ((==) "1") (stripNum track) =
                    [HH.tr_ [HH.th [colSpan width] [fa "circle", HH.text $ nbsp <> (fromMaybe "" $ stripNum disc)]], songRow i s]
                songOrDisc i s = [songRow i s]

                songRow i s@(Song { title, track, time, artist, disc }) =
                  HH.tr (if maybe false ((==) s) state.currentSong then [toClass "info"] else [])
                  $ A.catMaybes [ Just $ HH.td_ [HH.text $ fromMaybe "" $ stripNum track]
                                , if various then Just $ HH.td_ [HH.text artist] else Nothing
                                , Just $ HH.td [clickable, onClickDo $ PlayAlbum album i] [HH.text title]
                                , Just $ HH.td_ [HH.text $ formatTime time]
                                , Just $ HH.td [clickable, onClickDo $ AddSong s] [fa "plus-circle"]
                                ]

                footer = HH.tr_ $ 
                    [ HH.td [colSpan $ width - 2] []
                    , HH.td_ [HH.text $ formatTime $ sum $ map (\(Song s) -> s.time) songs]
                    ]



child :: forall eff. Artist -> Unit -> { component :: Component State Query (Aff (AppEffects eff)), initialState :: State }
child artist = \_ -> { component: ui, initialState: init artist }
