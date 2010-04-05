{-# OPTIONS -O2 -Wall #-}

module TextView(textView) where

import qualified Graphics.Vty as Vty
import VtyWrap(vtyString)
import Data.Monoid(mempty)
import Vector2(Vector2(..))
import Widget(Widget, WidgetFields(WidgetFields))

textView :: Vty.Attr -> Widget String
textView attr text = WidgetFields image (Just ((`div` 2) `fmap` size)) mempty
  where
    image = vtyString attr text
    size = Vector2 (Vty.image_width image) (Vty.image_height image)
