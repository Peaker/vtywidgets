{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.SizeRange
    (Size,
     SizeRange(..), inSizeRange, atMinSize, atMaxSize, atBothSizes,
     make, fixedSize, expanding, horizontallyExpanding, verticallyExpanding,
     )
where

import Data.Function.Utils(Endo)
import Data.Vector.Vector2(Vector2(..))
import Data.Monoid(Monoid(..))
import Control.Applicative(pure)

type Size = Vector2 Int
data SizeRange = SizeRange {
  srMinSize :: Size,
  srMaxSize :: Size
  }
  deriving (Eq, Ord, Show, Read)
inSizeRange :: Endo Size -> Endo Size -> Endo SizeRange
inSizeRange minFunc maxFunc (SizeRange minSize maxSize) =
  SizeRange (minFunc minSize) (maxFunc maxSize)
atBothSizes :: Endo Size -> Endo SizeRange
atBothSizes f = f `inSizeRange` f
atMinSize :: Endo Size -> Endo SizeRange
atMinSize = (`inSizeRange` id)
atMaxSize :: Endo Size -> Endo SizeRange
atMaxSize = (id `inSizeRange`)
fixedSize :: Size -> SizeRange
fixedSize size = SizeRange size size

instance Monoid SizeRange where
  mempty = fixedSize (pure 0)
  SizeRange minSize1 maxSize1 `mappend` SizeRange minSize2 maxSize2 =
    make (max minSize1 minSize2) (max maxSize1 maxSize2)

make :: Size -> Size -> SizeRange
make minSize maxSize = SizeRange minSize (max minSize maxSize)

maxBoundHack :: (Bounded a, Integral a) => a
maxBoundHack = maxBound `div` 25

horizontallyExpanding :: Int -> Int -> SizeRange
horizontallyExpanding fixedHeight minWidth = SizeRange (Vector2 minWidth fixedHeight)
                                                       (Vector2 maxBoundHack fixedHeight)
verticallyExpanding :: Int -> Int -> SizeRange
verticallyExpanding fixedWidth minHeight = SizeRange (Vector2 fixedWidth minHeight)
                                                     (Vector2 fixedWidth maxBoundHack)

expanding :: Int -> Int -> SizeRange
expanding minWidth minHeight = SizeRange (Vector2 minWidth minHeight)
                                         (pure maxBoundHack)
