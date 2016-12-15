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
    driver (left $ action Update)
    repeat 1000 $ driver (left $ action Update)


repeat :: forall e a. Int -> Aff e a -> Aff e Unit
repeat ms a = later' ms $ do
  a
  repeat ms a
