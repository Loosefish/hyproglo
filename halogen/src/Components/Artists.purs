module Components.Artists
  ( Query(..)
  , State(..)
  , init
  , Slot(..)
  , child
  ) where

import Hpg.Prelude

import Data.Array as A
import Data.Array ((:))
import Data.String as S
import Data.NonEmpty as N

import Halogen as HA
import Halogen.HTML.Indexed as H

import Model (Artist(..), AppUi, AppUpdate, AppChild)
import Mpd (fetchAlbumArtists)
import Util (viewLiA, toClass)


data Query a = LoadArtists a
type State = Array Artist

init :: State
init = []


eval :: AppUpdate Query State
eval (LoadArtists next) = do
    HA.set =<< HA.fromAff fetchAlbumArtists
    pure next


render :: State -> HA.ComponentHTML Query
render artists = outer $ header : map artistGroup artistsByLetter
  where
    outer = H.div [toClass "col-xs-12 col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"]
    header = H.h4 [toClass "page-header"] [H.text "Album Artists"]
    artistGroup group = H.span_ $
        [ H.text $ letter $ N.head group
        , H.ul [toClass "nav nav-pills"] $ map artistLink $ A.fromFoldable group
        ]
    artistLink (Artist { name }) = viewLiA ("#/music/" <> name) [H.text name]
    artistsByLetter = A.groupBy (eqBy letter) artists
    letter (Artist { name }) = S.toUpper $ S.take 1 name


-- Component

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


ui :: AppUi State Query
ui = HA.lifecycleComponent
    { render
    , eval
    , initializer: Just $ HA.action LoadArtists
    , finalizer: Nothing
    }


child :: AppChild State Query
child _ = { component: ui, initialState: init }
