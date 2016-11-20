module Components.App where

import Hpg.Prelude

import Data.Array ((:))

import Halogen as H
import Halogen (Component, ChildF, ParentDSL, ParentHTML, ParentState)
import Halogen.Component.ChildPath (cpL, cpR)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (href)

import Model (Artist(..), AppEffects)
import Components.Artists as CAR
import Components.Albums as CAL
import Util (toClass, nbsp, fa)


type State = View
data View = Artists | Albums Artist
data Query a = SetView View a

type ChildState = Either CAR.State CAL.State
type ChildQuery = Coproduct CAR.Query CAL.Query
type ChildSlot = Either CAR.Slot CAL.Slot

type State' g = ParentState State ChildState Query ChildQuery g ChildSlot
type Query' = Coproduct Query (ChildF ChildSlot ChildQuery)


ui :: forall eff. Component (State' (Aff (AppEffects eff))) Query' (Aff (AppEffects eff))
ui = H.parentComponent { render, eval, peek: Nothing }
  where
    eval :: Query ~> ParentDSL State ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
    eval (SetView view next) = do
        H.set view
        pure next

    render :: State -> ParentHTML ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
    render view =
        HH.div [toClass "container-fluid"] 
        [ HH.div [toClass "row"]
            [ HH.div [toClass "col-xs-12 col-sm-3 col-md-2 sidebar"] sidebar
            , child view
            ]
        ]
      where
        sidebar =
            [ HH.h4 [toClass "page-header"] [HH.text $ "HyProGlo" <> nbsp]
            , HH.ul [toClass "nav nav-sidebar"] [musicLink]
            , HH.div_ []
            ]

        musicLink = navLink active "#/music" $ (fa "database fa-fw") : (HH.text $ nbsp <> "Music") : extra
          where
            active = case view of
                _ -> true
            extra = case view of
                Albums (Artist { name }) -> [HH.text $ nbsp <> "/" <> name]
                _ -> []

        navLink active target content =
            HH.li (if active then [toClass "active"] else []) [HH.a [href target] content]

        child Artists = HH.slot' cpL CAR.Slot \_ -> { component: CAR.ui, initialState: [] }
        child (Albums artist) = HH.slot' cpR CAL.Slot \_ -> { component: CAL.ui, initialState: CAL.init artist}
