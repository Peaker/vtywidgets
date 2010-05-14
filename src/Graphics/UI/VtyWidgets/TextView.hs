{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.TextView
    (make)
where

import qualified Graphics.Vty as Vty
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.Display as Display
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange

make :: Vty.Attr -> String -> Display a
make attr text = Display.make (SizeRange.fixedSize size) (const . const $ image)
  where
    image = TermImage.string attr text
    size = TermImage.stringSize text
