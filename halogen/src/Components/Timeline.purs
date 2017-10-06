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

import Model (Song(..), Album(..), AppEffects, albumUrl, artistName)
import Mpd as M
import Util

data Query a = LoadAlbums a | LoadSongs (Array Album) a | Play Album a | Add Album a
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
    byDate <- A.sortWith (\(Album a) -> a.date) <$> HA.liftAff M.fetchAllAlbums
    songs <- HA.liftAff $ sequence $ map M.fetchSongs $ A.take 20 byDate
    HA.put { albums: A.zip byDate songs, busy: false }
    eval $ LoadSongs (A.drop 20 byDate) next

eval (LoadSongs [] next) = pure next

eval (LoadSongs albums next) = do
    songs <- HA.liftAff $ sequence $ map M.fetchSongs $ A.take 50 albums
    loaded <- HA.gets _.albums
    HA.modify (_ { albums =  loaded <> A.zip albums songs })
    eval $ LoadSongs (A.drop 50 albums) next

eval (Play album next) = do
    _ <- HA.liftAff $ M.sendCmds [M.Clear, M.AddAlbum album, M.Play Nothing]
    HA.raise unit
    pure next

eval (Add album next) = do
    _ <- HA.liftAff $ M.sendCmd $ M.AddAlbum album
    HA.raise unit
    pure next


render :: State -> HA.ComponentHTML Query
render { busy, albums } =
    H.div [cls "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] <% do
        put $ H.h4 [cls "page-header"] <% do
           put $ H.text "Timeline"
           when busy $ puts [H.text nbsp, fa "circle-o-notch fa-spin"]
        puts $ map yearGroup byYear
  where
    decade (Album { date }) = S.take 3 date <> "0s"
    year (Album { date }) = S.take 4 date
    byDecade = A.groupBy (eqBy (decade <<< fst)) albums
    byYear = A.groupBy (eqBy (year <<< fst)) albums

    yearGroup group = H.div [cls "row"] <% do
        put $ H.h5 [cls "col-xs-12"] [H.text thisYear]
        puts $ A.drop 1 $ A.concat $ A.mapWithIndex albumOrClear $ A.fromFoldable group
        put $ H.div [cls "clearfix"] []
        put $ H.div [cls "col-xs-1"] []
        put $ H.div [cls "col-xs-10"] [H.hr_]
        put $ H.div [cls "col-xs-1"] []
      where
        thisYear = year $ fst $ N.head group

        albumOrClear i a
            | i `mod` 4 == 0 = [H.div [cls "clearfix"] [], albumCol a]
            | i `mod` 2 == 0 = [H.div [cls "clearfix visible-xs-block"] [], albumCol a]
            | otherwise = [albumCol a]

        albumCol (Tuple album@(Album a) songs) = H.div [cls "col-xs-6 col-sm-3"] <% do
            put $ H.div [cls "thumbnail"] <% do
                maybePut image $ A.head songs
                put $ H.div [cls "caption text-center"] <% do
                    put $ H.h6_ <% do
                        put $ H.text $ a.title <> ensp
                        put $ H.span [cls "fa fa-play-circle clickable", onClickDo $ Play album] []
                        put $ H.text nbsp
                        put $ H.span [cls "fa fa-plus-circle clickable", onClickDo $ Add album] []
                    put $ H.span_ [H.text $ intercalate " " [artistName a.artist, emdash, a.date]]
          where
            image (Song { file }) =
                H.a [href $ albumUrl album] [H.img [src $ "/image/" <> file, cls "img-responsive center-block"]]
