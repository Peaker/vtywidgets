module TextView(textView) where

import qualified Graphics.Vty as Vty
import VtyWrap(vtyString)
import Data.Monoid(mempty)
import Vector2(Vector2(..))
import Widget(Widget, WidgetFields(WidgetFields))

textView :: Vty.Attr -> Widget String
textView attr text = WidgetFields image (Just (Vector2 0 0)) mempty
  where
    image = vtyString attr text
