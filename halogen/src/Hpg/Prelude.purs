module Hpg.Prelude
    ( module Prelude
    , module Control.Alt
    , module Control.Monad.Aff
    , module Control.Monad.Eff
    , module Control.Monad.Eff.Console
    , module Data.Either
    , module Data.Foldable
    , module Data.Functor.Coproduct
    , module Data.List
    , module Data.Maybe
    , module Data.String
    , module Data.Tuple
    ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)

import Data.Either (Either(..))
import Data.Foldable (sum, and, or, all, any)
import Data.Functor.Coproduct (Coproduct(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..), fst, snd)
