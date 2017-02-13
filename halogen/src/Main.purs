module Main where

import Hpg.Prelude

import Control.Monad.Aff (forkAff, later')
import Data.Functor.Coproduct (left)

import Halogen (parentState, runUI, action)
import Halogen.Util (awaitBody, runHalogenAff)

import Model (AppEffects, statusPlayState, PlayState(..))
import Router (routeSignal)
import Components.App (ui, Query(..), init)
import Mpd as M
import Network.HTTP.Affjax (AJAX)


main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI ui (parentState init) body
    forkAff $ routeSignal driver
    updater (driver <<< left <<< action)


updater :: forall a e q. ((q -> Query q) -> Aff (ajax :: AJAX | a) e) -> Aff (ajax :: AJAX | a) Unit
updater appAction = forkLater 0 update
  where
    forkLater i = void <<< forkAff <<< later' i
    update = do 
        (Tuple status song) <- M.fetchStatusSong
        appAction $ Update status song
        delay <- case statusPlayState <$> status of
            Just Playing -> pure 1000
            Just _ -> do
                M.queryMpd "idle"
                pure 100
            Nothing -> pure 5000
        forkLater delay update
