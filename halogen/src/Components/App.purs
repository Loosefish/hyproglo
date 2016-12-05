module Components.App where

import Hpg.Prelude

import Data.Array (catMaybes, singleton)

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
import Util (toClass, nbsp, fa, trimDate, onClickDo, formatTime, styleProp)
import Mpd (queryMpd, fetchStatusSong)


type State =
    { view :: View
    , song :: Maybe Song
    , status :: Maybe Status
    }

data View = Artists | Albums Artist | Playlist
derive instance eqView :: Eq View
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
render { status, song, view } =
    HH.div [toClass "container-fluid"] <% do
        put $ HH.div [toClass "row"] <% do
            put $ HH.div [toClass "col-xs-12 col-sm-3 col-md-2 sidebar"] <% do
                put $ HH.h4 [toClass "page-header"] [HH.text $ "HyProGlo" <> nbsp]
                put $ HH.ul [toClass "nav nav-sidebar"] <% do
                    musicLink
                    playlistLink
                put $ HH.div_ <% do
                    maybePut id (songInfo <$> status <*> song)
                    maybePut controls status
            put $ child view
  where
    musicLink = navLink (view /= Playlist) "#/music" <% do
        put $ fa "database fa-fw"
        put $ (HH.text $ nbsp <> "Music")
        case view of
            Albums (Artist { name }) -> put $ HH.text $ nbsp <> "/" <> nbsp <> name
            _ -> pure unit

    playlistLink = navLink (view == Playlist) "#/playlist" <% do
        put $ fa "list fa-fw"
        put $ HH.text $ nbsp <> "Playlist"
        maybePut playlistBadge status
      where
        playlistBadge (Status s) = HH.span [toClass "badge pull-right"] [HH.text $ show $ s.playlistLength]

    navLink active target content = put $
        HH.li (if active then [toClass "active"] else []) [HH.a [href target] content]

    controls (Status { random, repeat, single, playState }) = HH.div_ <% do
        buttonGroup <% do
            if playState == Playing
                then button false (SendCmd "pause") [fa "pause"]
                else button false (SendCmd "play") [fa "play"]
        buttonGroup <% do
            button false (SendCmd "previous") [fa "fast-backward"]
            button false (SendCmd "stop") [fa "stop"]
            button false (SendCmd "next") [fa "fast-forward"]
        buttonGroup  <% do
            button random (SendCmd $ "random " <> if random then "0" else "1") [fa "random"]
            button repeat (SendCmd $ "repeat " <> if repeat then "0" else "1") [fa "repeat"]
            button single (SendCmd $ "single " <> if single then "0" else "1") [HH.text "1"]
      where
        buttonGroup = put <<<
            HH.div [toClass "btn-group btn-group-sm btn-group-justified"]
        button active action = put <<<
            HH.a [onClickDo action, toClass $ "btn btn-default" <> if active then " active" else ""]

    songInfo (Status { time }) (Song { file, title, artist, album }) = HH.div_ <% do
        put $ HH.img [toClass "img-responsive hidden-xs center-block", src $ "/image/" <> file]
        put $ HH.a (catMaybes [albumsUrl <$> album]) <% do
            put $ HH.h5 [toClass "text-center"] [HH.text title]
            put $ HH.h6 [toClass "text-center"] [HH.text artist]
            maybePut albumInfo album
        maybePut progress time
      where
        albumsUrl (Album a) = href $ "#/music/" <> artistName a.artist
        albumInfo (Album a) =
            HH.h6 [toClass "text-center"] <% do
                put $ HH.text a.title
                put $ HH.small_ [HH.text $ nbsp <> "(" <> trimDate a.date <> ")"]

        progress (Tuple elapsed total) =
            HH.div [toClass "progress"] $ singleton $
                HH.div [toClass "progress-bar", percent] $
                    map HH.text [formatTime elapsed, nbsp <> "/" <> nbsp, formatTime total ]
          where
            percent = styleProp $ "width:" <> show ((elapsed * 100) / total) <> "%;"

    child Artists = HH.slot' pathArtists CAR.Slot CAR.child
    child (Albums artist) = HH.slot' pathAlbums CAL.Slot $ CAL.child artist
    child Playlist = HH.slot' pathPlaylist CP.Slot CP.child
