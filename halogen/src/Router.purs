module Router where

import Hpg.Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Functor.Coproduct (left)

import DOM (DOM)

import Halogen (Driver, action)

import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

import Model (Artist(..))
import Components.App (View(..), Query', Query(..))


routing :: Match View
routing = 
    (\name -> Albums (Artist { name })) <$> (lit "" *> lit "music" *> str)
    <|> Artists <$ (lit "" *> lit "music")
    <|> pure Artists


type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)


routeSignal :: forall eff. Driver Query' eff -> Aff (Effects eff) Unit
routeSignal driver = do
    Tuple old new <- matchesAff routing
    driver $ left $ action $ SetView new
