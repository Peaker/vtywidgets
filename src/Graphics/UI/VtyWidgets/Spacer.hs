{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Spacer
    (make, makeHorizontal, makeVertical)
where

import Control.Applicative(pure)
import Data.Monoid(mempty)
import qualified Graphics.UI.VtyWidgets.Widget as Widget
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))

maxBoundHack :: (Bounded a, Integral a) => a
maxBoundHack = maxBound `div` 2

make :: Widget.SizeRange -> Widget.Display a
make sizeRange = Widget.Display sizeRange mempty

makeHorizontal :: Widget.Display a
makeHorizontal = make $ Widget.makeSizeRange (pure 0) (Vector2 maxBoundHack 0)

makeVertical :: Widget.Display a
makeVertical = make $ Widget.makeSizeRange (pure 0) (Vector2 0 maxBoundHack)
