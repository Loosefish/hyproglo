module Main where

import Hpg.Prelude

import Control.Monad.Aff (forkAff)

import Halogen (parentState, runUI)
import Halogen.Util (awaitBody, runHalogenAff)

import Model (AppEffects)
import Router (routeSignal)
import Components.App (View(..), ui)


main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI ui (parentState Artists) body
    forkAff $ routeSignal driver
