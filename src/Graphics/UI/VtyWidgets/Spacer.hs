{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Spacer
    (make, makeHorizontal, makeVertical)
where

import Control.Applicative(pure)
import Data.Monoid(mempty)
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.VtyWidgets.SizeRange(SizeRange)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.Display(Display)
import qualified Graphics.UI.VtyWidgets.Display as Display

maxBoundHack :: (Bounded a, Integral a) => a
maxBoundHack = maxBound `div` 2

make :: SizeRange -> Display a
make sizeRange = Display.make sizeRange mempty

makeHorizontal :: Display a
makeHorizontal = make $ SizeRange.make (pure 0) (Vector2 maxBoundHack 0)

makeVertical :: Display a
makeVertical = make $ SizeRange.make (pure 0) (Vector2 0 maxBoundHack)
