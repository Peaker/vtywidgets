module TextView(textView) where

import qualified Graphics.Vty as Vty
import VtyWrap(vtyString)
import Data.Monoid(mempty)
import Widget(Widget, WidgetFields(WidgetFields))

textView :: Vty.Attr -> Widget String
textView attr text = WidgetFields image mempty
  where
    image = vtyString attr text
