module Components.Artists where

import Hpg.Prelude

import Data.Array as A
import Data.Array ((:))
import Data.String as S

import Halogen (Component, ComponentDSL, ComponentHTML)
import Halogen as H
import Halogen.HTML.Indexed as HH
{-- import Halogen.HTML.Properties.Indexed (href) --}

import Model (Artist(..), AppEffects)
import Mpd (fetchAlbumArtists)
import Util (viewLiA, toClass)


type State = Array Artist


data Query a = LoadArtists a
data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = H.lifecycleComponent
    { render
    , eval
    , initializer: Just $ H.action LoadArtists
    , finalizer: Nothing
    }
  where
    eval :: Query ~> ComponentDSL State Query (Aff (AppEffects eff))
    eval (LoadArtists next) = do
        artists <- H.fromAff fetchAlbumArtists
        H.set artists
        pure next

    render :: State -> ComponentHTML Query
    render artists = 
        HH.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] $
        (HH.h4 [toClass "page-header"] [HH.text "Album Artists"])
        : (map (artistGroup <<< A.fromFoldable) $ A.groupBy (\a b -> letter a == letter b) artists)
      where
        artistLink a@(Artist { name }) = viewLiA ("#/music/" <> name) [HH.text name]

        artistGroup group = HH.span_ $ A.catMaybes
            [ HH.text <<< letter <$> A.head group
            , Just $ HH.ul [toClass "nav nav-pills"] $ map artistLink group
            ]

        letter (Artist { name }) = S.toUpper $ S.take 1 name
