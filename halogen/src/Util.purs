module Util where

import Hpg.Prelude

import Data.Char (fromCharCode)
import Data.String as S

import DOM.Event.Types (MouseEvent)

import Unsafe.Coerce (unsafeCoerce)

import Halogen.HTML.Core (HTML, Prop, ClassName(..), prop, PropName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML as H
import Halogen.HTML.Properties (IProp, classes, class_)


toClass :: forall r i. String -> IProp (class :: String | r) i
toClass s = classes $ map ClassName $ S.split (Pattern " ") s


fa :: forall p i. String -> HTML p i
fa name = H.span [toClass $ "fa fa-" <> name] []


nbsp :: String
nbsp = S.singleton $ fromCharCode 160


stripNum :: Maybe String -> Maybe String
stripNum text = S.takeWhile ((/=) '/') <$> text


trimDate :: String -> String
trimDate = S.takeWhile ((/=) '-')


formatTime :: Int -> String
formatTime total = show minutes <> ":" <> pad seconds
  where
    minutes = total / 60
    seconds = total `mod` 60
    pad s = (if s < 10 then "0" else "") <> show s


clickable :: forall r i. IProp (class :: String | r) i
clickable  = class_ $ ClassName "clickable"


onClickDo :: forall a b. (Unit -> b Unit) -> IProp ( onClick :: MouseEvent | a) (b Unit)
onClickDo = HE.onClick <<< HE.input_


styleProp :: forall r i. String -> IProp (style :: String | r) i
styleProp = refine $ prop (PropName "style")
  where
    refine :: forall a r' i'. (a -> Prop i') -> a -> IProp r' i'
    refine = unsafeCoerce
