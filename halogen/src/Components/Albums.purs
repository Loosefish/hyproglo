module Components.Albums where

import Hpg.Prelude

import Data.Array as A
import Data.Array ((:))
import Data.String as S

import DOM.HTML.Types (WINDOW)

import Halogen (ComponentHTML)
import Halogen as HA
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (src, colSpan, id_)

import Model (Artist, Album(..), Song(..), albumId, songAlbum, AppUi, AppUpdate, AppChild, artistName, songArtist, songDisc)
import Mpd (sendCmd, sendCmds, fetchAlbums')
import Mpd as M
import Util (toClass, fa, clickable, nbsp, stripNum, formatTime, onClickDo)

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


render :: State -> ComponentHTML Query
render { artist, albums, busy, currentSong } =
    H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] <% do
        put header
        when (A.length albums > 1) (puts [links, H.hr_])
        puts $ map albumRow albums
  where
    header = H.h4 [toClass "page-header clickable"] <% do 
        put $ H.text $ artistName artist
        when busy $ puts [H.text nbsp, fa "circle-o-notch fa-spin"]

    links = H.ul [toClass $ "nav nav-pills"] $ map albumLink albums
      where
        albumLink (Tuple album _) = H.li
            [ toClass $ "clickable" <> if (songAlbum =<< currentSong) == Just album then " active" else ""
            , onClickDo $ Focus album
            ]
            [H.a_ <% albumTitle album]

    albumTitle (Album { title, date }) = do
        put $ H.text $ title <> nbsp
        put $ H.small_ $ map H.text ["(", S.take 4 date, ")"]

    albumRow (Tuple album songs) =
        H.div [id_ $ albumId album,toClass "row"] <% do
            put $ H.div [toClass "col-xs-12"] <% do
                put $ H.h5_ <% do
                    put $ H.span [clickable, onClickDo $ PlayAlbum album 0] <% albumTitle album
                    put $ H.text nbsp
                    put $ fa "plus-circle clickable"
            put $ H.div [toClass "col-xs-12 col-sm-8"] [songTable]
            maybePut image $ A.head songs
      where
        image (Song { file }) =
            H.div [toClass "col-sm-4 col-xs-hidden"] $ A.singleton $
                H.img [src $ "/image/" <> file, toClass "img-responsive center-block"]

        songTable = H.table [toClass "table table-condensed table-hover"] <% do
            put $ H.tbody_ $ A.concat $ A.mapWithIndex songOrDisc songs
            put $ H.tfoot_ [footer]
          where
            various = any (notEq (artistName artist) <<< songArtist) songs
            multidisc = 1 < A.length (A.nub $ A.mapMaybe songDisc songs)
            width = if various then 5 else 4

            songOrDisc i s@(Song { track, disc })
                | multidisc && maybe false ((==) "1") (stripNum track) = 
                    [ H.tr_ $ A.singleton $ H.th [colSpan width] <% do
                        put $ fa "circle"
                        put $ H.text $ nbsp <> (fromMaybe "" $ stripNum disc)
                    , songRow i s
                    ]
                | otherwise = [songRow i s]

            songRow i s@(Song { title, track, time, disc }) =
                H.tr (if currentSong == Just s then [toClass "info"] else []) <% do
                    put $ H.td_ [H.text $ fromMaybe "" $ stripNum track]
                    when various $ put $ H.td_ [H.text $ songArtist s]
                    put $ H.td [clickable, onClickDo $ PlayAlbum album i] [H.text title]
                    put $ H.td_ [H.text $ formatTime time]
                    put $ H.td [clickable, onClickDo $ AddSong s] [fa "plus-circle"]

            footer = H.tr_ <% do 
                put $ H.td [colSpan $ width - 2] []
                put $ H.td_ [H.text $ formatTime $ sum $ map (\(Song s) -> s.time) songs]


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
