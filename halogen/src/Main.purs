module Main where

import Hpg.Prelude

import Control.Monad.Aff (forkAff, later')
import Data.Functor.Coproduct (left)

import Halogen (parentState, runUI, action)
import Halogen.Util (awaitBody, runHalogenAff)

import Model (AppEffects)
import Router (routeSignal)
import Components.App (ui, Query(..), init)


main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI ui (parentState init) body
    forkAff $ routeSignal driver
    setInterval 1000 $ driver (left $ action Update)


setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
