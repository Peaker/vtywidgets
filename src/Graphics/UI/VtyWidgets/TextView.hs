{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TextView
    (make)
where

import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import qualified Graphics.UI.VtyWidgets.Widget as Widget

make :: Vty.Attr -> String -> Widget.Display a
make attr text = Widget.Display (Widget.fixedSize size) (const . const $ image)
  where
    image = TermImage.string attr text
    size = TermImage.stringSize text
