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

import Halogen (ComponentDSL)
import Halogen as HA
import Halogen.HTML as H
import Halogen.HTML.Properties (href)

import Network.HTTP.Affjax (AJAX)

import Model (Artist(..), AppEffects)
import Mpd (fetchAlbumArtists, fetchAllAlbums)
import Mpd as M
import Util (toClass, clickable, fa, nbsp, onClickDo)

data Query a = LoadArtists a | PlayRandomAlbum a
type State = Array Artist
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
    , initialState: const []
    , receiver: const Nothing
    }


eval :: forall eff. Query ~> ComponentDSL State Query Unit (Aff (AppEffects eff))
eval (LoadArtists next) = do
    HA.put =<< HA.liftAff fetchAlbumArtists
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


render :: State -> HA.ComponentHTML Query
render artists =
    H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] <% do
        put $ H.h4 [toClass "page-header"] [H.text "Album Artists"]
        put $ H.ul [toClass "nav nav-pills"] <% do
            put $ H.li_ [H.a [clickable, onClickDo PlayRandomAlbum] [fa "random", H.text $ nbsp <> "Play random album"]]
        put $ H.hr_
        puts $ map artistGroup artistsByLetter
  where
    artistGroup group = H.span_ <% do
        put $ H.text $ letter $ N.head group
        put $ H.ul [toClass "nav nav-pills"] $ map artistLink $ A.fromFoldable group
    artistLink (Artist { name }) =
        H.li_ [H.a [href $ "#/music/" <> name] [H.text name]]
    artistsByLetter = A.groupBy (eqBy letter) artists
    letter (Artist { name }) = S.toUpper $ S.take 1 name
