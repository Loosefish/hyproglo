module Components.App where

import Hpg.Prelude

import Data.Array (catMaybes, singleton)
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.String as S

import DOM.HTML.Types (WINDOW)

import Halogen as HA
import Halogen (Component, ParentDSL, ParentHTML)
import Halogen.Query (query')
import Halogen.Component.ChildPath (cp1, cp2, cp3, cp4)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (href, src)

import Components.Albums as CAL
import Components.Artists as CAR
import Components.Playlist as CP
import Components.Timeline as CT
import Model (AppEffects, Album(..), Artist(..), PlayState(..), Song(..), Status(..), statusPlaylistSong, statusPlaylist, albumUrl)
import Util (cls, nbsp, fa, trimDate, onClickDo, formatTime, styleProp)
import Mpd (queryMpd, fetchStatusSong)

foreign import setTitle :: forall eff. String -> Eff (window :: WINDOW | eff) Unit


type State =
    { view :: View
    , song :: Maybe Song
    , status :: Maybe Status
    }

data View = Artists | Albums Artist (Maybe Album) | Playlist | Timeline
derive instance eqView :: Eq View
data Query a
    = SetView View a
    | GetUpdate a
    | Update (Maybe Status) (Maybe Song) a
    | SendCmd String a


-- Compound types
type ChildQuery = Coproduct4 CAR.Query CAL.Query CP.Query CT.Query
type ChildSlot = Either4 CAR.Slot CAL.Slot CP.Slot CT.Slot


init :: State
init =
    { view: Artists
    , song: Nothing
    , status : Nothing
    }


component :: forall eff. Component HTML Query Unit Void (Aff (AppEffects eff))
component = HA.parentComponent
    { render
    , eval
    , initialState: const init
    , receiver: const Nothing
    }


eval :: forall eff. Query ~> ParentDSL State Query ChildQuery ChildSlot Void (Aff (AppEffects eff))
eval (SetView view next) = do
    void $ case view of
        Albums artist album -> query' cp2 CAL.Slot (HA.action $ CAL.SetArtist artist album)
        _ -> pure Nothing
    HA.modify (_ { view = view})
    pure next

eval (GetUpdate next) = do
    (Tuple status song) <- HA.liftAff $ fetchStatusSong
    eval $ Update status song next

eval (Update status song next) = do
    -- Update playlist if changed
    oldStatus <- HA.gets _.status
    when (eqBy (map statusPlaylist) oldStatus status)
        (void $ query' cp3 CP.Slot $ HA.action CP.GetPlaylist)
    -- Update title
    HA.liftEff $ setTitle $ docTitle song status
    -- Update local state
    HA.modify (_ { status = status, song = song})
    pure next
  where
    docTitle (Just (Song { artist, title })) (Just (Status { playState })) =
        if playState == Stopped
            then "HyProGlo"
            else S.joinWith " - " [title, artist, "HyProGlo"]
    docTitle _ _ = "HyProGlo"

eval (SendCmd cmd next) = do
    void $ HA.liftAff $ queryMpd cmd
    eval (GetUpdate next)


render :: forall eff. State -> ParentHTML Query ChildQuery ChildSlot (Aff (AppEffects eff))
render { status, song, view } =
    H.div [cls "container-fluid"] <% do
        put $ H.div [cls "row"] <% do
            put $ H.div [cls "col-xs-12 col-sm-3 col-md-2 sidebar"] <% do
                put $ H.h4 [cls "page-header"] [H.text $ "HyProGlo" <> nbsp]
                put $ H.ul [cls "nav nav-sidebar"] <% do
                    put musicLink
                    put timelineLink
                    put playlistLink
                put $ H.div_ <% do
                    maybePut id (songInfo <$> status <*> song)
                    maybePut controls status
            put $ child view
  where
    musicLink = navLink (view `notElem` [Playlist, Timeline]) "#/music" <% do
        put $ fa "database fa-fw"
        put $ (H.text $ nbsp <> "Music")
        maybePut H.text $ case view of
            Albums (Artist { name }) _ -> Just $ nbsp <> "/" <> nbsp <> name
            _ -> Nothing

    playlistLink = navLink (view == Playlist) "#/playlist" <% do
        put $ fa "list fa-fw"
        put $ H.text $ nbsp <> "Playlist"
        maybePut playlistBadge status
      where
        playlistBadge (Status s) = H.span [cls "badge pull-right"] [H.text $ show $ s.playlistLength]

    timelineLink = navLink (view == Timeline) "#/timeline" <% do
        put $ fa "clock-o"
        put $ H.text $ nbsp <> "Timeline"

    navLink active target content =
        H.li (if active then [cls "active"] else []) [H.a [href target] content]

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
            H.div [cls "btn-group btn-group-sm btn-group-justified"]
        button active action = put <<<
            H.a [onClickDo action, cls $ "btn btn-default" <> if active then " active" else ""]

    songInfo (Status { time }) (Song { file, title, artist, album }) = H.div_ <% do
        put $ H.img [cls "img-responsive hidden-xs center-block", src $ "/image/" <> file]
        put $ H.a (catMaybes [href <<< albumUrl <$> album]) <% do
            put $ H.h5 [cls "text-center"] [H.text title]
            put $ H.h6 [cls "text-center"] [H.text artist]
            maybePut albumInfo album
        maybePut progress time
      where
        albumInfo (Album a) =
            H.h6 [cls "text-center"] <% do
                put $ H.text a.title
                put $ H.small_ [H.text $ " (" <> trimDate a.date <> ")"]

        progress (Tuple elapsed total) =
            H.div [cls "progress"] $ singleton $
                H.div [cls "progress-bar", percent] $
                    map H.text [formatTime elapsed, nbsp <> "/" <> nbsp, formatTime total ]
          where
            percent = styleProp $ "width:" <> show ((elapsed * 100) / total) <> "%;"

    child Artists = H.slot' cp1 CAR.Slot CAR.component unit peek
    child (Albums artist album) = H.slot' cp2 CAL.Slot (CAL.component artist album) song peek
    child Playlist = H.slot' cp3 CP.Slot CP.component (statusPlaylistSong =<< status) peek
    child Timeline = H.slot' cp4 CT.Slot CT.component unit peek


peek :: Unit -> Maybe (Query Unit)
peek _ = Just $ HA.action $ GetUpdate
