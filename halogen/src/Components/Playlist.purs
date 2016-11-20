module Components.Playlist where

import Hpg.Prelude

import Data.Array as A

import Halogen (Component, ComponentDSL, ComponentHTML)
import Halogen as H
import Halogen.HTML.Indexed as HH

import Model (Song, AppEffects)
import Util (toClass)


newtype State = State { playlist :: Array Song }


data Query a = LoadPlaylist a
data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = H.lifecycleComponent
    { render
    , eval
    , initializer: Just $ H.action LoadPlaylist
    , finalizer: Nothing
    }
  where
    eval :: Query ~> ComponentDSL State Query (Aff (AppEffects eff))
    eval (LoadPlaylist next) = do
        pure next

    render :: State -> ComponentHTML Query
    render (State { playlist }) = 
        HH.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"]
            [ HH.h4 [toClass "page-header"] [HH.text "Playlist"]
            , HH.div_ [HH.text $ show $ A.length playlist]
            ]
