module Router where

import Hpg.Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)

import DOM (DOM)

import Halogen (Driver, action)

import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

import Model


routing :: Match View
routing = 
    Playlist <$ (lit "" *> lit "playlist")
    <|> (\name -> Albums (Artist { name }) []) <$> (lit "" *> lit "music" *> str)
    <|> AlbumArtists <$ (lit "" *> lit "music")
    <|> pure AlbumArtists


type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)


routeSignal :: forall eff. Driver Query eff -> Aff (Effects eff) Unit
routeSignal driver = do
    Tuple old new <- matchesAff routing
    driver $ action $ ChangeView new
