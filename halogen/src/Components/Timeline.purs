module Components.Timeline where

import Hpg.Prelude

import Data.Array as A
import Data.String as S
import Data.NonEmpty as N

import Halogen (ComponentDSL)
import Halogen as HA
import Halogen.HTML as H
import Halogen.HTML.Properties (href, src)

import Network.HTTP.Affjax (AJAX)

import Model (Song(..), Album(..), AppEffects, albumUrl)
import Mpd (fetchAllAlbums, fetchSongs)
import Util (toClass, clickable, fa, nbsp, onClickDo)

data Query a = LoadAlbums a | LoadSongs (Array Album) a
type State =
    { albums :: Array (Tuple Album (Array Song))
    , busy :: Boolean
    }
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
    , initialState: const { albums: [], busy: false }
    , receiver: const Nothing
    }


eval :: forall eff. Query ~> ComponentDSL State Query Unit (Aff (AppEffects eff))
eval (LoadAlbums next) = do  -- Fetch albums and load the first chunk of songs
    HA.modify (_ { busy = true })
    byDate <- A.sortWith (\(Album a) -> a.date) <$> HA.liftAff fetchAllAlbums
    songs <- HA.liftAff $ sequence $ map fetchSongs $ A.take 20 byDate
    HA.put { albums: A.zip byDate songs, busy: false }
    eval $ LoadSongs (A.drop 20 byDate) next

eval (LoadSongs [] next) = pure next

eval (LoadSongs albums next) = do
    songs <- HA.liftAff $ sequence $ map fetchSongs $ A.take 50 albums
    loaded <- HA.gets _.albums
    HA.modify (_ { albums =  loaded <> A.zip albums songs })
    eval $ LoadSongs (A.drop 50 albums) next


render :: State -> HA.ComponentHTML Query
render { busy, albums } =
    H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] <% do
        put $ H.h4 [toClass "page-header"] <% do
           put $ H.text "Timeline"
           when busy $ puts [H.text nbsp, fa "circle-o-notch fa-spin"]
        puts $ map yearGroup byYear
  where
    decade (Album { date }) = S.take 3 date <> "0s"
    year (Album { date }) = S.take 4 date
    byDecade = A.groupBy (eqBy (decade <<< fst)) albums
    byYear = A.groupBy (eqBy (year <<< fst)) albums

    yearGroup group = H.div [toClass "row"] <% do
        put $ H.h5 [toClass "col-xs-12"] [H.text thisYear]
        puts $ A.drop 1 $ A.concat $ A.mapWithIndex albumOrClear $ A.fromFoldable group
        put $ H.div [toClass "clearfix"] []
        put $ H.div [toClass "col-xs-1"] []
        put $ H.div [toClass "col-xs-10"] [H.hr_]
        put $ H.div [toClass "col-xs-1"] []
      where
        thisYear = year $ fst $ N.head group

        albumOrClear i a
            | i `mod` 4 == 0 = [H.div [toClass "clearfix"] [], albumCol a]
            | i `mod` 2 == 0 = [H.div [toClass "clearfix visible-xs-block"] [], albumCol a]
            | otherwise = [albumCol a]

        albumCol (Tuple (Album a) songs) = H.a [href $ albumUrl (Album a), toClass "col-xs-6 col-sm-3"] <% do
            maybePut image $ A.head songs
            put $ H.h6 [toClass "text-center"] <% do
                put $ H.text $ a.title <> nbsp
                put $ H.p_ [H.small_ $ map H.text ["(", a.date, ")"]]

        image (Song { file }) = H.img [src $ "/image/" <> file, toClass "img-responsive center-block"]
