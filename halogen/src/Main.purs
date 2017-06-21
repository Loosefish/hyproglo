module Main where

import Hpg.Prelude

import Control.Monad.Aff (forkAff, delay)
import Data.Time.Duration (Milliseconds(..))

import Halogen (action)
import Halogen.VDom.Driver (runUI)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Aff.Util (awaitBody, runHalogenAff)

import Model (AppEffects, statusPlayState, PlayState(..))
import Router (routeSignal)
import Components.App (component, Query(..))
import Mpd as M
import Network.HTTP.Affjax (AJAX)


main :: Eff (HalogenEffects (AppEffects ())) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI component unit body
    _ <- forkAff $ routeSignal driver.query
    updater (driver.query <<< action)


updater :: forall a e q. ((q -> Query q) -> Aff (ajax :: AJAX | a) e) -> Aff (ajax :: AJAX | a) Unit
updater appAction = void $ forkAff update
  where
    update = do 
        (Tuple status song) <- M.fetchStatusSong
        _ <- appAction $ Update status song
        waitFor <- case statusPlayState <$> status of
            Just Playing -> pure 1000.0
            Just _ -> do
                _ <- M.queryMpd "idle"
                pure 100.0
            Nothing -> pure 5000.0
        delay $ Milliseconds waitFor
        void $ forkAff update
