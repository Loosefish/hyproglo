module Util where

import Hpg.Prelude

import Data.Array ((:))
import Data.Char (fromCharCode)
import Data.String as S

import Halogen.HTML.Core (className, HTML(Element))
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (classes, href, IProp(..), I)


toClass :: forall r i. String -> IProp (class :: I | r) i
toClass s = classes $ map className $ S.split (Pattern " ") s


addProp :: forall r p i. IProp r i -> HTML p i -> HTML p i
addProp (IProp p) (Element n t ps cs) = Element n t (p : ps) cs
addProp _ element = element


fa :: forall p i. String -> HTML p i
fa name = H.span [toClass $ "fa fa-" <> name] []


nbsp :: String
nbsp = S.singleton $ fromCharCode 160


viewLiA :: forall a q. String -> Array (HTML a (q Unit)) -> HTML a (q Unit)
viewLiA h c = H.li_ [H.a [href h] c]


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


onClickDo = HE.onClick <<< HE.input_

dirname = S.takeWhile ((/=) '/')  -- Only handles depth == 1
