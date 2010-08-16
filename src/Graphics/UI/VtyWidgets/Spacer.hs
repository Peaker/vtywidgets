{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Spacer
    (make, makeHorizontal, makeVertical, makeFixed, makeWidthSpace, makeHeightSpace,
     indent)
where

import           Control.Applicative              (Applicative(..))
import           Data.Vector.Vector2              (Vector2(..))
import           Data.Monoid                      (mempty)
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange, Size)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import           Graphics.UI.VtyWidgets.Display   (Display)
import qualified Graphics.UI.VtyWidgets.Display   as Display
import qualified Graphics.UI.VtyWidgets.Box       as Box

make :: Applicative f => SizeRange -> Display f
make sizeRange = Display.make sizeRange (const $ pure mempty)

makeFixed :: Applicative f => Size -> Display f
makeFixed = make . SizeRange.fixedSize

makeHorizontal :: Applicative f => Display f
makeHorizontal = make $ SizeRange.horizontallyExpanding 0 0

makeVertical :: Applicative f => Display f
makeVertical = make $ SizeRange.verticallyExpanding 0 0

makeWidthSpace :: Applicative f => Int -> Display f
makeWidthSpace width = makeFixed $ Vector2 width 0

makeHeightSpace :: Applicative f => Int -> Display f
makeHeightSpace height = makeFixed $ Vector2 0 height

indent :: Applicative f => Int -> Display f -> Display f
indent width disp = Box.makeView Box.Horizontal [makeWidthSpace width, disp]
