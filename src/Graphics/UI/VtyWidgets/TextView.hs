{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TextView
    (make)
where

import qualified Graphics.Vty as Vty
import Data.Monoid(mempty)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.Widget(Widget(..))

make :: Vty.Attr -> String -> Widget ()
make attr text = Widget image (Just ((`div` 2) `fmap` size)) mempty
  where
    image = TermImage.string attr text
    size = TermImage.stringSize text
