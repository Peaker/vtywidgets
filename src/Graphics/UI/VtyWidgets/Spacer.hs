{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Spacer
    (make, makeHorizontal, makeVertical)
where

import           Data.Monoid                      (mempty)
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import           Graphics.UI.VtyWidgets.Display   (Display)
import qualified Graphics.UI.VtyWidgets.Display   as Display

make :: SizeRange -> Display a
make sizeRange = Display.make sizeRange mempty

makeHorizontal :: Display a
makeHorizontal = make $ SizeRange.horizontallyExpanding 0 0

makeVertical :: Display a
makeVertical = make $ SizeRange.verticallyExpanding 0 0
