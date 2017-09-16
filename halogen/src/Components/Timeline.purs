module Components.Timeline where

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

import Model (Album(..), AppEffects)
import Mpd (fetchAllAlbums)
import Mpd as M
import Util (toClass, clickable, fa, nbsp, onClickDo)

data Query a = LoadAlbums a
type State = Array Album
type Effects eff = (ajax :: AJAX | eff)
data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


{-- component :: forall eff. Component HTML Query Void Unit (Aff (AppEffects eff)) --}
component = HA.lifecycleComponent
    { render
    , eval
    , initializer: Just $ HA.action LoadAlbums
    , finalizer: Nothing
    , initialState: const []
    , receiver: const Nothing
    }


eval :: forall eff. Query ~> ComponentDSL State Query Unit (Aff (AppEffects eff))
eval (LoadAlbums next) = do
    HA.put =<< HA.liftAff fetchAllAlbums
    pure next


render :: State -> HA.ComponentHTML Query
render albums =
    H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] <% do
        put $ H.h4 [toClass "page-header"] [H.text "Timeline"]
        puts $ map albumRow (A.sortWith (\(Album a) -> a.date) albums)
  where
    albumRow (Album a) =
        H.p [] [H.text a.title, H.text a.date]
