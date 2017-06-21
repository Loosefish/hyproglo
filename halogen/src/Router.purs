module Router where

import Hpg.Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)

import DOM (DOM)

import Halogen (action)

import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

import Model (Artist(..), Album(..))
import Components.App (View(..), Query(..))


routing :: Match View
routing = 
    view <$> (lit "" *> lit "music" *> str) <*> str <*> str
    <|> (\name -> Albums (Artist { name }) Nothing) <$> (lit "" *> lit "music" *> str)
    <|> Artists <$ (lit "" *> lit "music")
    <|> Playlist <$ (lit "" *> lit "playlist")
    <|> pure Artists
  where
    view name date title = let artist = Artist { name } in
        Albums artist $ Just $ Album { artist, date, title }


type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)


routeSignal :: forall e. (Query Unit -> Aff e Unit) -> Aff e Unit
routeSignal query = do
    Tuple old new <- matchesAff routing
    query $ action $ SetView new
