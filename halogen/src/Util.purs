module Util where

import Hpg.Prelude

import Data.Char (fromCharCode)
import Data.String as S

import Unsafe.Coerce (unsafeCoerce)

import Halogen.HTML.Core (HTML, Prop, attrName, className, prop, propName)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (I, IProp, classes)


toClass :: forall r i. String -> IProp (class :: I | r) i
toClass s = classes $ map className $ S.split (Pattern " ") s


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


clickable :: forall r i. IProp (class :: I | r) i
clickable  = toClass "clickable"


onClickDo :: forall a b. (Unit -> b Unit) -> IProp ( onClick :: I | a) (b Unit)
onClickDo = HE.onClick <<< HE.input_


styleProp :: forall r i. String -> IProp (style :: I | r) i
styleProp = refine $ prop (propName "style") (Just $ attrName "style")
  where
    refine :: forall a r' i'. (a -> Prop i') -> a -> IProp r' i'
    refine = unsafeCoerce
