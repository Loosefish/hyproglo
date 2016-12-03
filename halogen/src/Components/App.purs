module Components.App where

import Hpg.Prelude

import Data.Array ((:), catMaybes, singleton)

import Halogen as H
import Halogen (Component, ChildF, ParentDSL, ParentHTML, ParentState)
import Halogen.Component (query')
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (href, src)

import Components.Albums as CAL
import Components.Artists as CAR
import Components.Playlist as CP
import Model (AppEffects, Album(..), Artist(..), PlayState(..), Song(..), Status(..), artistName)
import Util (toClass, nbsp, fa, trimDate, onClickDo, formatTime, styleProp, (<%), one, whenM, many)
import Mpd (queryMpd, fetchStatusSong)


type State =
    { view :: View
    , song :: Maybe Song
    , status :: Maybe Status
    }

data View = Artists | Albums Artist | Playlist
data Query a
    = SetView View a
    | Update a
    | SendCmd String a
data Flag = Random | Repeat | Single


-- Compound types
type ChildState = Either CAR.State (Either CAL.State CP.State)
type ChildQuery = Coproduct CAR.Query (Coproduct CAL.Query CP.Query)
type ChildSlot = Either CAR.Slot (Either CAL.Slot CP.Slot)

type State' g = ParentState State ChildState Query ChildQuery g ChildSlot
type Query' = Coproduct Query (ChildF ChildSlot ChildQuery)

-- Child paths
pathArtists :: ChildPath CAR.State ChildState CAR.Query ChildQuery CAR.Slot ChildSlot
pathArtists = cpL

pathAlbums :: ChildPath CAL.State ChildState CAL.Query ChildQuery CAL.Slot ChildSlot
pathAlbums = cpR :> cpL

pathPlaylist :: ChildPath CP.State ChildState CP.Query ChildQuery CP.Slot ChildSlot
pathPlaylist = cpR :> cpR


init :: State
init =
    { view: Artists
    , song: Nothing
    , status : Nothing
    }


ui :: forall eff. Component (State' (Aff (AppEffects eff))) Query' (Aff (AppEffects eff))
ui = H.parentComponent { render, eval, peek: Nothing }


eval :: forall eff. Query ~> ParentDSL State ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
eval (SetView (Albums artist) next) = do
    query' pathAlbums CAL.Slot (H.action $ CAL.SetArtist artist)
    H.modify (_ { view = Albums artist })
    pure next

eval (SetView view next) = do
    H.modify (_ { view = view})
    pure next

eval (Update next) = do
    (Tuple status song) <- H.fromAff $ fetchStatusSong
    H.modify (_ { status = status, song = song})
    query' pathAlbums CAL.Slot (H.action $ CAL.SetCurrentSong song)
    pure next

eval (SendCmd cmd next) = do
    H.fromAff $ queryMpd cmd
    eval (Update next)


render :: forall eff. State -> ParentHTML ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
render state =
    HH.div [toClass "container-fluid"] <% do
        one $ HH.div [toClass "row"] <% do
            one $ HH.div [toClass "col-xs-12 col-sm-3 col-md-2 sidebar"] <% do
                one $ HH.h4 [toClass "page-header"] [HH.text $ "HyProGlo" <> nbsp]
                one $ HH.ul [toClass "nav nav-sidebar"] [musicLink, playlistLink state.status]
                one $ HH.div_ $ catMaybes [songInfo <$> state.status <*> state.song, controls <$> state.status]
            one $ child state.view
  where
    musicLink = navLink active "#/music" $ (fa "database fa-fw") : (HH.text $ nbsp <> "Music") : extra
      where
        active = case state.view of
            Playlist -> false
            _ -> true
        extra = case state.view of
            Albums (Artist { name }) -> [HH.text $ nbsp <> "/" <> nbsp <> name]
            _ -> []

    playlistLink status =
        navLink active "#/playlist" <% do
            one $ fa "list fa-fw"
            one $ HH.text $ nbsp <> "Playlist"
            whenM (one <<< playlistBadge) status
      where
        playlistBadge (Status { playlistLength }) =
            HH.span [toClass "badge pull-right"] [HH.text $ show $ playlistLength]
        active = case state.view of
            Playlist -> true
            _ -> false

    navLink active target content =
        HH.li (if active then [toClass "active"] else []) [HH.a [href target] content]

    controls (Status { random, repeat, single, playState }) = HH.div_ <% do
        buttonGroup <% do
            case playState of
                Playing -> button false (SendCmd "pause") [fa "pause"]
                _ -> button false (SendCmd "play") [fa "play"]
        buttonGroup <% do
            button false (SendCmd "previous") [fa "fast-backward"]
            button false (SendCmd "stop") [fa "stop"]
            button false (SendCmd "next") [fa "fast-forward"]
        buttonGroup  <% do
            button random (SendCmd $ "random " <> if random then "0" else "1") [fa "random"]
            button repeat (SendCmd $ "repeat " <> if repeat then "0" else "1") [fa "repeat"]
            button single (SendCmd $ "single " <> if single then "0" else "1") [HH.text "1"]
      where
        buttonGroup = one <<< HH.div [toClass "btn-group btn-group-sm btn-group-justified"]
        button active action = one <<< HH.a [onClickDo action, toClass $ "btn btn-default" <> if active then " active" else ""]

    songInfo (Status { time }) (Song { file, title, artist, album }) = HH.div_ <% do
        one $ HH.img [toClass "img-responsive hidden-xs center-block", src $ "/image/" <> file]
        one $ HH.a (catMaybes [albumsUrl <$> album]) <% do
            one $ HH.h5 [toClass "text-center"] [HH.text title]
            one $ HH.h6 [toClass "text-center"] [HH.text artist]
            whenM (one <<< albumInfo) album
        whenM (one <<< progress) time
      where
        albumsUrl (Album { artist }) = href $ "#/music/" <> artistName artist
        albumInfo (Album a) =
            HH.h6 [toClass "text-center"] <% do
                one $ HH.text a.title
                one $ HH.small_ [HH.text $ nbsp <> "(" <> trimDate a.date <> ")"]

        progress (Tuple elapsed total) =
            HH.div [toClass "progress"] $ singleton $
                HH.div [toClass "progress-bar", percent] $
                    map HH.text [formatTime elapsed, nbsp <> "/" <> nbsp, formatTime total ]
          where
            percent = styleProp $ "width:" <> show ((elapsed * 100) / total) <> "%;"

    child Artists = HH.slot' pathArtists CAR.Slot CAR.child
    child (Albums artist) = HH.slot' pathAlbums CAL.Slot $ CAL.child artist
    child Playlist = HH.slot' pathPlaylist CP.Slot CP.child

