module Components.Playlist where

import Hpg.Prelude

import Data.Array as A

import Halogen (Component, ComponentDSL, ComponentHTML)
import Halogen as HA
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (colSpan)

import Model (Album(..), Song(..), AppEffects)
import Util (toClass, clickable, fa, formatTime, onClickDo)
import Mpd as M


type State = { playlist :: Array Song, currentSong :: Maybe Int }


data Query a
    = GetPlaylist a
    | Play Int a
    | Delete Int a
    | Clear a
    | SetCurrentSong (Maybe Int) a
data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = HA.lifecycleComponent
    { render
    , eval
    , initializer: Just $ HA.action GetPlaylist
    , finalizer: Nothing
    }


eval :: forall eff. Query ~> ComponentDSL State Query (Aff (AppEffects eff))
eval (GetPlaylist next) = do
    playlist <- HA.fromAff M.fetchPlaylistSongs
    HA.modify (_ { playlist = playlist })
    pure next
eval (Play i next) = do
    HA.fromAff $ M.sendCmd $ M.Play $ Just i
    HA.modify (_ { currentSong = Just i })
    pure next
eval (Delete i next) = do
    HA.fromAff $ M.sendCmd $ M.Delete i
    playlist <- HA.gets _.playlist
    HA.modify (_ { playlist = fromMaybe playlist $ A.deleteAt i playlist})
    pure next
eval (Clear next) = do
    HA.fromAff $ M.sendCmd M.Clear
    HA.modify (_ { playlist = [] })
    pure next
eval (SetCurrentSong currentSong next) = do
    HA.modify (_ { currentSong = currentSong })
    pure next


render :: State -> ComponentHTML Query
render { playlist, currentSong } = 
    H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] <% do
        put $ H.h4 [toClass "page-header"] [H.text "Playlist"]
        when (A.length playlist > 0) (put playlistTable)
  where
    playlistTable = H.table [toClass "table table-condensed table-hover"] <% do
        put $ H.thead_ <% do
            put $ H.tr_ <% do
                puts $ map (H.th_ <<< A.singleton <<< H.text) ["#", "Title", "Artist", "Album", "Time"]
                put $ H.th_ [H.span [clickable, onClickDo Clear] [fa "times-circle fa-lg"]]
        put $ H.tbody_ $ A.mapWithIndex playlistRow playlist
        put $ H.tfoot_ $ A.singleton $ H.tr_ <% do
            put $ H.td [colSpan 4] []
            put $ H.td_ [H.text $ formatTime $ sum $ map (\(Song s) -> s.time) playlist]

    playlistRow i (Song s) = H.tr (if (Just i) == currentSong then [toClass "info"] else []) <% do
        put $ H.td_ [H.text $ show $ i + 1]
        put $ H.td [clickable, onClickDo $ Play i] [H.text s.title]
        put $ H.td_ [H.text s.artist]
        put $ H.td_ $ case s.album of
            Just (Album a) -> [H.text a.title]
            _ -> []
        put $ H.td_ [H.text $ formatTime s.time]
        put $ H.td_ [H.span [clickable, onClickDo $ Delete i] [fa "minus-circle fa-lg"]]


child :: forall eff. Maybe Int -> Unit -> { component :: Component State Query (Aff (AppEffects eff)), initialState :: State }
child i _ =
    { component: ui
    , initialState: { playlist: [], currentSong: i} 
    }


