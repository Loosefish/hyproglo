module Components.Artists
  ( Query(..)
  , State(..)
  , Slot(..)
  , child
  ) where

import Hpg.Prelude

import Control.Monad.Eff.Random (randomInt)

import Data.Array as A
import Data.String as S
import Data.NonEmpty as N

import Halogen as HA
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (href)

import Model (Artist(..), AppUpdate, AppChild)
import Mpd (fetchAlbumArtists, fetchAllAlbums)
import Mpd as M
import Util (toClass, clickable, fa, nbsp, onClickDo)

data Query a = LoadArtists a | PlayRandomAlbum a
type State = Array Artist


eval :: AppUpdate Query State
eval (LoadArtists next) = do
    HA.set =<< HA.fromAff fetchAlbumArtists
    pure next

eval (PlayRandomAlbum next) = do
    albums <- HA.fromAff fetchAllAlbums
    index <- HA.fromEff $ randomInt 0 ((A.length albums) - 1)
    case A.index albums index of
        Just album -> do 
            HA.fromAff $ M.sendCmds [M.Clear, M.AddAlbum album, M.Play Nothing]
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


-- Component

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


child :: AppChild State Query
child _ = { component: ui, initialState: [] }
  where
    ui = HA.lifecycleComponent
        { render
        , eval
        , initializer: Just $ HA.action LoadArtists
        , finalizer: Nothing
        }
