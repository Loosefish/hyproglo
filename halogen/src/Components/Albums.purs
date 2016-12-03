module Components.Albums where

import Hpg.Prelude

import Control.Monad.Reader

import Data.Array as A
import Data.Array ((:))
import Data.String as S

import DOM.HTML.Types (WINDOW)

import Halogen (ComponentHTML)
import Halogen as HA
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (src, colSpan, id_)

import Model (Artist(..), Album(..), Song(..), albumId, songAlbum, AppUi, AppUpdate, AppChild)
import Mpd (sendCmd, sendCmds, fetchAlbums')
import Mpd as M
import Util (toClass, fa, clickable, nbsp, addProp, stripNum, formatTime, onClickDo, empty)

foreign import scrollToId :: forall eff. String -> Eff (window :: WINDOW | eff) Unit


data Query a
    = LoadAlbums a
    | Focus Album a
    | PlayAlbum Album Int a
    | AddSong Song a
    | SetArtist Artist a
    | SetCurrentSong (Maybe Song) a

type State =
    { artist :: Artist
    , albums :: Array (Tuple Album (Array Song))
    , busy :: Boolean
    , currentSong :: Maybe Song
    }


eval :: AppUpdate Query State
eval (LoadAlbums next) = do
    HA.modify (_ { busy = true })
    withSongs <- HA.fromAff <$> fetchAlbums' =<< HA.gets _.artist
    HA.modify (_ { albums = withSongs, busy = false })
    pure next
eval (Focus album next) = do
    HA.fromEff $ scrollToId $ albumId album
    pure next
eval (PlayAlbum album i next) = do
    HA.fromAff $ sendCmds [M.Clear, M.AddAlbum album, M.Play $ Just i]
    pure next
eval (AddSong song next) = do
    HA.fromAff $ sendCmd $ M.AddSong song
    pure next
eval (SetArtist artist next) = do
    HA.modify (_ {artist = artist})
    eval (LoadAlbums next)
eval (SetCurrentSong song next) =
    HA.modify (_ {currentSong = song}) $> next



header :: forall p i. State -> HA.HTML p i
header state = H.h4 [toClass "page-header clickable"] $ 
    (H.text $ (\(Artist a) -> a.name) state.artist <> nbsp) : if state.busy then [fa "circle-o-notch fa-spin"] else []

headerR :: forall p i. Reader State (HA.HTML p i)
headerR = do
    state <- ask
    pure $ H.h4 [toClass "page-header clickable"] $ 
      (H.text $ (\(Artist a) -> a.name) state.artist <> nbsp) : if state.busy then [fa "circle-o-notch fa-spin"] else []
  

render :: State -> ComponentHTML Query
render state =
    H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] $
    [header state, links, H.hr_] <> map albumRow albums
  where
    name = (\(Artist { name }) -> name) state.artist
    albums = state.albums

    links | A.length albums == 0 = empty
          | otherwise = H.ul [toClass $ "nav nav-pills"] $ map albumLink albums

    albumLink (Tuple album _) = H.li
        [ toClass $ "clickable" <> if (songAlbum =<< state.currentSong) == Just album then " active" else ""
        , onClickDo $ Focus album
        ]
        [H.a_ $ albumTitle album]

    albumTitle (Album { title, date }) = [H.text $ title <> nbsp, H.small_ $ map H.text ["(", S.take 4 date, ")"]]

    albumRow (Tuple album songs) =
        H.div [id_ $ albumId album,toClass "row"]
            [ H.div [toClass "col-xs-12"] [H.h5_ [H.span [clickable, onClickDo $ PlayAlbum album 0] $ albumTitle album, H.text nbsp, addProp (clickable) $ fa "plus-circle"]]
            , H.div [toClass "col-xs-12 col-sm-8"] $ [songTable]
            , H.div [toClass "col-sm-4 col-xs-hidden"] [image $ A.head songs]
            ]
        where
        image Nothing = H.img []
        image (Just (Song { file })) =
            H.img [src $ "/image/" <> file, toClass "img-responsive center-block"]

        songTable = H.table [toClass "table table-condensed table-hover"]
            [ H.tbody_ $ A.concat $ A.mapWithIndex songOrDisc songs
            , H.tfoot_ [footer]
            ]
            where
            various = any (\(Song { artist }) -> artist /= name) songs
            multidisc = 1 < A.length (A.nub $ map (\(Song s) -> s.disc) songs)
            width = if various then 5 else 4

            songOrDisc i s@(Song { track, disc }) | multidisc && maybe false ((==) "1") (stripNum track) =
                [H.tr_ [H.th [colSpan width] [fa "circle", H.text $ nbsp <> (fromMaybe "" $ stripNum disc)]], songRow i s]
            songOrDisc i s = [songRow i s]

            songRow i s@(Song { title, track, time, artist, disc }) =
                H.tr (if state.currentSong == Just s then [toClass "info"] else [])
                $ A.catMaybes
                    [ Just $ H.td_ [H.text $ fromMaybe "" $ stripNum track]
                    , if various then Just $ H.td_ [H.text artist] else Nothing
                    , Just $ H.td [clickable, onClickDo $ PlayAlbum album i] [H.text title]
                    , Just $ H.td_ [H.text $ formatTime time]
                    , Just $ H.td [clickable, onClickDo $ AddSong s] [fa "plus-circle"]
                    ]

            footer = H.tr_ $ 
                [ H.td [colSpan $ width - 2] []
                , H.td_ [H.text $ formatTime $ sum $ map (\(Song s) -> s.time) songs]
                ]





-- Component

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


ui :: AppUi State Query
ui = HA.lifecycleComponent
    { render
    , eval
    , initializer: Just $ HA.action LoadAlbums
    , finalizer: Nothing
    }


child :: Artist -> AppChild State Query
child artist = \_ -> { component: ui, initialState: init }
  where
    init = 
        { artist: artist
        , albums: []
        , busy: false
        , currentSong: Nothing
        }
