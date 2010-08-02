{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Spacer
    (make, makeHorizontal, makeVertical, makeFixed, makeWidthSpace, makeHeightSpace,
     indent)
where

import           Data.Vector.Vector2              (Vector2(..))
import           Data.Monoid                      (mempty)
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange, Size)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import           Graphics.UI.VtyWidgets.Display   (Display)
import qualified Graphics.UI.VtyWidgets.Display   as Display
import qualified Graphics.UI.VtyWidgets.Box       as Box

make :: SizeRange -> Display a
make sizeRange = Display.make sizeRange mempty

makeFixed :: Size -> Display a
makeFixed = make . SizeRange.fixedSize

makeHorizontal :: Display a
makeHorizontal = make $ SizeRange.horizontallyExpanding 0 0

makeVertical :: Display a
makeVertical = make $ SizeRange.verticallyExpanding 0 0

makeWidthSpace :: Int -> Display a
makeWidthSpace width = makeFixed $ Vector2 width 0

makeHeightSpace :: Int -> Display a
makeHeightSpace height = makeFixed $ Vector2 0 height

indent :: Int -> Display a -> Display a
indent width disp = Box.makeView Box.Horizontal [makeWidthSpace width, disp]
