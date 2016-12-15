module Components.App where

import Hpg.Prelude

import Data.Array (catMaybes, singleton)
import Data.String as S

import Halogen as HA
import Halogen (Component, ChildF, ParentDSL, ParentHTML, ParentState)
import Halogen.Component (query')
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (href, src)

import Components.Albums as CAL
import Components.Artists as CAR
import Components.Playlist as CP
import Model (AppEffects, Album(..), Artist(..), PlayState(..), Song(..), Status(..), artistName, statusPlaylistSong, statusPlaylist)
import Util (toClass, nbsp, fa, trimDate, onClickDo, formatTime, styleProp)
import Mpd (queryMpd, fetchStatusSong)


type State =
    { view :: View
    , song :: Maybe Song
    , status :: Maybe Status
    }

data View = Artists | Albums Artist (Maybe Album) | Playlist
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
ui = HA.parentComponent { render, eval, peek: Just peek }


eval :: forall eff. Query ~> ParentDSL State ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
eval (SetView (Albums artist album) next) = do
    query' pathAlbums CAL.Slot (HA.action $ CAL.SetArtist artist album)
    HA.modify (_ { view = Albums artist album })
    pure next

eval (SetView view next) = do
    HA.modify (_ { view = view})
    pure next

eval (Update next) = do
    (Tuple status song) <- HA.fromAff $ fetchStatusSong
    oldStatus <- HA.gets _.status
    when (eqBy (map statusPlaylist) oldStatus status)
        (void $ query' pathPlaylist CP.Slot $ HA.action CP.GetPlaylist)
    query' pathAlbums CAL.Slot (HA.action $ CAL.SetCurrentSong song)
    query' pathPlaylist CP.Slot (HA.action $ CP.SetCurrentSong $ statusPlaylistSong =<< status)
    HA.modify (_ { status = status, song = song})
    pure next

eval (SendCmd cmd next) = do
    HA.fromAff $ queryMpd cmd
    eval (Update next)


render :: forall eff. State -> ParentHTML ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
render { status, song, view } =
    H.div [toClass "container-fluid"] <% do
        put $ H.div [toClass "row"] <% do
            put $ H.div [toClass "col-xs-12 col-sm-3 col-md-2 sidebar"] <% do
                put $ H.h4 [toClass "page-header"] [H.text $ "HyProGlo" <> nbsp]
                put $ H.ul [toClass "nav nav-sidebar"] <% do
                    musicLink
                    playlistLink
                put $ H.div_ <% do
                    maybePut id (songInfo <$> status <*> song)
                    maybePut controls status
            put $ child view
  where
    musicLink = navLink (view /= Playlist) "#/music" <% do
        put $ fa "database fa-fw"
        put $ (H.text $ nbsp <> "Music")
        case view of
            Albums (Artist { name }) _ -> put $ H.text $ nbsp <> "/" <> nbsp <> name
            _ -> pure unit

    playlistLink = navLink (view == Playlist) "#/playlist" <% do
        put $ fa "list fa-fw"
        put $ H.text $ nbsp <> "Playlist"
        maybePut playlistBadge status
      where
        playlistBadge (Status s) = H.span [toClass "badge pull-right"] [H.text $ show $ s.playlistLength]

    navLink active target content = put $
        H.li (if active then [toClass "active"] else []) [H.a [href target] content]

    controls (Status { random, repeat, single, playState }) = H.div_ <% do
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
            button single (SendCmd $ "single " <> if single then "0" else "1") [H.text "1"]
      where
        buttonGroup = put <<<
            H.div [toClass "btn-group btn-group-sm btn-group-justified"]
        button active action = put <<<
            H.a [onClickDo action, toClass $ "btn btn-default" <> if active then " active" else ""]

    songInfo (Status { time }) (Song { file, title, artist, album }) = H.div_ <% do
        put $ H.img [toClass "img-responsive hidden-xs center-block", src $ "/image/" <> file]
        put $ H.a (catMaybes [albumsUrl <$> album]) <% do
            put $ H.h5 [toClass "text-center"] [H.text title]
            put $ H.h6 [toClass "text-center"] [H.text artist]
            maybePut albumInfo album
        maybePut progress time
      where
        albumsUrl (Album a) = href $ S.joinWith "/" ["#", "music", artistName a.artist, a.date, a.title]
        albumInfo (Album a) =
            H.h6 [toClass "text-center"] <% do
                put $ H.text a.title
                put $ H.small_ [H.text $ " (" <> trimDate a.date <> ")"]

        progress (Tuple elapsed total) =
            H.div [toClass "progress"] $ singleton $
                H.div [toClass "progress-bar", percent] $
                    map H.text [formatTime elapsed, nbsp <> "/" <> nbsp, formatTime total ]
          where
            percent = styleProp $ "width:" <> show ((elapsed * 100) / total) <> "%;"

    child Artists = H.slot' pathArtists CAR.Slot CAR.child
    child (Albums artist album) = H.slot' pathAlbums CAL.Slot $ CAL.child artist album song
    child Playlist = H.slot' pathPlaylist CP.Slot $ CP.child $ statusPlaylistSong =<< status


-- | Watch child queries to update on interesting events
peek (HA.ChildF _ q) = case q of
    Coproduct (Right (Coproduct (Left q'))) -> peekAlbums q'
    Coproduct (Right (Coproduct (Right q'))) -> peekPlaylist q'
    Coproduct (Left _) -> pure unit  -- artists
  where
    peekAlbums (CAL.PlayAlbum _ _ _) = eval $ Update unit
    peekAlbums (CAL.PlayArtist _) = eval $ Update unit
    peekAlbums _ = pure unit

    peekPlaylist (CP.Play _ _) = eval $ Update unit
    peekPlaylist (CP.Delete _ _) = eval $ Update unit
    peekPlaylist (CP.Clear _) = eval $ Update unit
    peekPlaylist _ = pure unit
