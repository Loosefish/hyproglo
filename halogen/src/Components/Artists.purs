module Components.Artists
  ( Query(..)
  , State(..)
  , Slot(..)
  , component
  ) where

import Hpg.Prelude

import Control.Monad.Eff.Random (randomInt)

import Data.Array as A
import Data.String as S
import Data.NonEmpty as N

import DOM.Event.KeyboardEvent as K
import DOM.Event.KeyboardEvent (KeyboardEvent(..))

import Halogen (ComponentDSL)
import Halogen as HA
import Halogen.HTML as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (ButtonType(ButtonButton))

import Network.HTTP.Affjax (AJAX)

import Model (Artist(..), Album(..), Song(..), AppEffects)
import Mpd (fetchAlbumArtists, fetchAllAlbums, searchAny, searchTitle)
import Mpd as M
import Util (toClass, clickable, fa, nbsp, onClickDo, trimDate, formatTime)

foreign import getSearchQuery :: Unit -> String
foreign import setSearchQuery :: String -> Unit


data Query a = LoadArtists a
             | Search a | SearchClear a | SearchKey KeyboardEvent a
             | PlayRandomAlbum a
             | PlaySong Song a | AddSong Song a
type State =
    { artists :: Array Artist
    , results :: Maybe (Array Song)
    }
type Effects eff = (ajax :: AJAX | eff)
data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


{-- component :: forall eff. Component HTML Query Void Unit (Aff (AppEffects eff)) --}
component = HA.lifecycleComponent
    { render
    , eval
    , initializer: Just $ HA.action LoadArtists
    , finalizer: Nothing
    , initialState: const { artists: [], results: Nothing }
    , receiver: const Nothing
    }


eval :: forall eff. Query ~> ComponentDSL State Query Unit (Aff (AppEffects eff))
eval (LoadArtists next) = do
    artists <- HA.liftAff fetchAlbumArtists
    HA.modify (_ { artists = artists })
    pure next

eval (PlayRandomAlbum next) = do
    albums <- HA.liftAff fetchAllAlbums
    index <- HA.liftEff $ randomInt 0 ((A.length albums) - 1)
    case A.index albums index of
        Just album -> do 
            _ <- HA.liftAff $ M.sendCmds [M.Clear, M.AddAlbum album, M.Play Nothing]
            HA.raise unit
            pure next
        _ -> pure next

eval (Search next) = do
    songs <- HA.liftAff $ searchTitle $ getSearchQuery unit
    HA.modify (_ { results = Just songs })
    pure next

eval (SearchClear next) = do
    pure $ setSearchQuery ""
    HA.modify (_ { results = Nothing })
    pure next

eval (SearchKey event next) = 
    case K.key event of
         "Enter" -> eval $ Search next
         "Escape" -> eval $ SearchClear next
         _ -> pure next

eval (PlaySong song next) = do
    _ <- HA.liftAff $ M.sendCmds [M.Clear, M.AddSong song, M.Play Nothing]
    HA.raise unit
    pure next

eval (AddSong song next) = do
    _ <- HA.liftAff $ M.sendCmd $ M.AddSong song
    HA.raise unit
    pure next



render :: State -> HA.ComponentHTML Query
render { artists, results } =
    H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] <% do
        put $ H.h4 [toClass "page-header"] [H.text "Music"]
        put searchBar
        put $ H.ul [toClass "nav nav-pills"] <% do
            put $ H.li_ [H.a [HP.href "#/timeline"] [fa "clock-o", H.text $ nbsp <> "Timeline"]]
            put $ H.li_ [H.a [clickable, onClickDo PlayRandomAlbum] [fa "random", H.text $ nbsp <> "Play random album"]]
        put $ H.hr_
        case results of
            Nothing -> puts $ map artistGroup artistsByLetter
            Just songs -> put $ resultTable songs
  where
    artistGroup group = H.span_ <% do
        put $ H.text $ letter $ N.head group
        put $ H.ul [toClass "nav nav-pills"] $ map artistLink $ A.fromFoldable group

    artistLink (Artist { name }) =
        H.li_ [H.a [HP.href $ "#/music/" <> name] [H.text name]]

    artistsByLetter = A.groupBy (eqBy letter) artists

    letter (Artist { name }) = S.toUpper $ S.take 1 name

    searchBar = H.p_ $ A.singleton $ H.div [toClass "input-group"] <% do
       put $ H.span [toClass "input-group-btn"]
           [H.button [toClass "btn btn-default", HP.type_ HP.ButtonButton, onClickDo SearchClear] [fa "times"]]
       put $ H.input [HP.id_ "query", toClass "form-control", HP.type_ HP.InputText, HP.placeholder "Search", HE.onKeyDown $ HE.input SearchKey]
       put $ H.span [toClass "input-group-btn"]
           [H.button [toClass "btn btn-default", HP.type_ ButtonButton, onClickDo Search] [fa "search"]]

    resultTable songs = H.table [toClass "table table-condensed table-hover"] <% do
        put $ H.thead_ <% do
            put $ H.tr_ <% do
                puts $ map (H.th_ <<< A.singleton <<< H.text) ["Title", "Artist", "Album", "Time"]
                put $ H.th_ []
        put $ H.tbody_ $ map resultRow songs

    resultRow song@(Song s) = H.tr_ <% do
        puts $ map (playTd <<< A.singleton <<< H.text) [s.title, s.artist]
        put $ playTd $ case s.album of
                   Just (Album a) -> [H.text $ a.title <> nbsp, H.em_ [H.text $ "(" <> trimDate a.date <> ")"]]
                   _ -> []
        put $ playTd [H.text $ formatTime s.time]
        put $ H.td_ [H.span [clickable, onClickDo $ AddSong song] [fa "plus-circle fa-lg"]]
      where
        playTd = H.td [toClass "clickable", onClickDo $ PlaySong song]
