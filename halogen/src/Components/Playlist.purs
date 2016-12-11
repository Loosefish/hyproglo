module Components.Playlist where

import Hpg.Prelude

import Data.Array as A

import Halogen (Component, ComponentDSL, ComponentHTML)
import Halogen as HA
import Halogen.HTML.Indexed as H

import Model (Album(..), Song(..), AppEffects)
import Util (toClass, clickable, fa, formatTime)
import Mpd as M


type State = { playlist :: Array Song }


data Query a = GetPlaylist a | PlaySong Int a
data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


init :: State
init = { playlist: [] } 


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
eval (PlaySong i next) = do
    HA.fromAff $ M.sendCmd $ M.Play $ Just i
    pure next

render :: State -> ComponentHTML Query
render { playlist } = 
    H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] <% do
        put $ H.h4 [toClass "page-header"] [H.text "Playlist"]
        when (A.length playlist > 0) (put playlistTable)
  where
    playlistTable = H.table [toClass "table table-condensed table-hover"] <% do
        put $ H.thead_ <% do
            put $ H.tr_ <% do
                puts $ map (H.th_ <<< A.singleton <<< H.text) ["#", "Title", "Artist", "Album", "Time"]
                put $ H.th_ [H.span [clickable] [fa "times-circle fa-lg"]]
        put $ H.tbody_ $ A.mapWithIndex playlistRow playlist

    playlistRow i (Song s) = H.tr_ <% do
        put $ H.td_ [H.text $ show $ i + 1]
        put $ H.td_ [H.text s.title]
        put $ H.td_ [H.text s.artist]
        put $ H.td_ $ case s.album of
            Just (Album a) -> [H.text a.title]
            _ -> []
        put $ H.td_ [H.text $ formatTime s.time]
        put $ H.td_ []




child :: forall eff. Unit -> { component :: Component State Query (Aff (AppEffects eff)), initialState :: State }
child _ = { component: ui, initialState: init }
