{-# OPTIONS -O2 -Wall #-}

module TextView(textView) where

import qualified Graphics.Vty as Vty
import Data.Monoid(mempty)
import qualified TermImage
import Widget(Widget, WidgetFields(WidgetFields))

textView :: Vty.Attr -> Widget String
textView attr text = WidgetFields image (Just ((`div` 2) `fmap` size)) mempty
  where
    image = TermImage.string attr text
    size = TermImage.stringSize text
