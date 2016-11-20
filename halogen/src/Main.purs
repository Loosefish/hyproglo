module Main where

import Hpg.Prelude

import Control.Monad.Aff (forkAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Parallel (parSequence)

import DOM.HTML (window)
import DOM.HTML.Types (WINDOW)
import DOM.HTML.Window (scroll)

import Data.Array (zip)

import Halogen (HalogenEffects(..), Component(..), ComponentDSL(..))
import Halogen as H
import Halogen.Util (runHalogenAff, awaitBody)

import Network.HTTP.Affjax (AJAX)

import Model
import Mpd (fetchAlbumArtists, fetchAlbums, fetchSongs)
import View (render)
import Router (routeSignal)


type AppEffects eff = H.HalogenEffects
    ( ajax :: AJAX
    , console :: CONSOLE
    , window :: WINDOW
    | eff)


main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- H.runUI appUi initialState body
    forkAff $ routeSignal driver


appUi :: forall eff. Component State Query (Aff (AppEffects eff))
appUi = H.lifecycleComponent
    { render
    , eval
    , initializer: Just (H.action FetchArtists)
    , finalizer: Nothing
    }
  where
    eval :: Query ~> ComponentDSL State Query (Aff (AppEffects eff))
    eval (FetchArtists next) = do
        H.modify (_ { busy = true })
        artists <- H.fromAff fetchAlbumArtists
        H.modify (_ { busy = false, albumArtists = artists})
        pure next
    eval (ChangeView (Albums artist _) next) = do
        H.modify (_ { busy = true })
        albums <- H.fromAff $ fetchAlbums artist
        songs <- H.fromAff <$> parSequence $ map fetchSongs albums
        let withSongs = zip albums songs
        H.modify (_ { busy = false, view = Albums artist withSongs})
        H.fromEff $ scrollTop
        pure next
    eval (ChangeView v next) = do
        H.modify (_ { view = v })
        H.fromEff $ scrollTop
        pure next
    
    scrollTop = scroll 0 0 =<< window
