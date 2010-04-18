{-# OPTIONS -O2 -Wall #-}

module TextView(textView) where

import qualified Graphics.Vty as Vty
import Data.Monoid(mempty)
import qualified TermImage
import Widget(Widget(Widget))

textView :: Vty.Attr -> String -> Widget ()
textView attr text = Widget image (Just ((`div` 2) `fmap` size)) mempty
  where
    image = TermImage.string attr text
    size = TermImage.stringSize text
