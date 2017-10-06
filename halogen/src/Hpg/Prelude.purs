module Hpg.Prelude
    ( module Prelude
    , module Control.Alt
    , module Control.Monad.Aff
    , module Control.Monad.Eff
    , module Control.Monad.Eff.Console
    , module Data.Either
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor.Coproduct
    , module Data.List
    , module Data.Maybe
    , module Data.String
    , module Data.Traversable
    , module Data.Tuple
    , eqBy
    , fromWriter, (<%), put, puts, maybePut, maybePuts
    ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Writer (Writer, execWriter, tell)

import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Foldable (sum, and, or, all, any, elem, notElem, surround, intercalate)
import Data.Function (on)
import Data.Functor.Coproduct (Coproduct(..), coproduct)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry, curry)

eqBy :: forall a b. (Eq b) => (a -> b) -> a -> a -> Boolean
eqBy = on (==)


-- | Pass the result a writer execution to an `outer` function.
fromWriter :: forall a w b. (w -> a) -> Writer w b -> a
fromWriter outer = outer <<< execWriter <<< void

infix 9 fromWriter as <%

put = tell <<< singleton
puts = tell

maybePut f m = maybe (pure unit) (put <<< f) m
maybePuts f m = maybe (pure unit) (puts <<< f) m
