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


main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI ui (parentState init) body
    forkAff $ routeSignal driver
    updater driver


updater driver = do
    (Tuple status song) <- M.fetchStatusSong
    driver $ left $ action $ Update status song
    case statusPlayState <$> status of
        Just Playing -> later' 1000 (updater driver) 
        Just _ -> do
            M.queryMpd "idle"
            later' 200 (updater driver)
        _ -> later' 5000 (updater driver) 
