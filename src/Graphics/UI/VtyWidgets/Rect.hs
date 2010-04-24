{-# OPTIONS -Wall -O2 #-}
module Graphics.UI.VtyWidgets.Rect
    (Coordinate, Rect(..),
     atTopLeft, atBottomRight, atBoth,
     ExpandingRect(..), inExpandingRect, inExpandingRect2,
     ShrinkingRect(..), inShrinkingRect, inShrinkingRect2
    )
where

import Data.Monoid(Monoid(..))
import Control.Applicative(pure, liftA2)
import Graphics.UI.VtyWidgets.Vector2(Vector2)

type Endo a = a -> a

type Coordinate = Vector2 Int
data Rect = Rect { topLeft :: Coordinate,
                   bottomRight :: Coordinate }
atTopLeft :: Endo Coordinate -> Endo Rect
atTopLeft f (Rect tl br) = Rect (f tl) br
atBottomRight :: Endo Coordinate -> Endo Rect
atBottomRight f (Rect tl br) = Rect tl (f br)
atBoth :: Endo Coordinate -> Endo Rect
atBoth f (Rect tl br) = Rect (f tl) (f br)

combineRect2 :: (Coordinate -> Coordinate -> Coordinate) ->
                (Coordinate -> Coordinate -> Coordinate) ->
                Rect -> Rect -> Rect
combineRect2 ftl fbr (Rect tl1 br1) (Rect tl2 br2) =
  Rect (tl1 `ftl` tl2) (br1 `fbr` br2)

minBoundHack :: (Bounded a, Integral a) => a
minBoundHack = minBound `div` 2

maxBoundHack :: (Bounded a, Integral a) => a
maxBoundHack = maxBound `div` 2

newtype ExpandingRect = ExpandingRect { unExpandingRect :: Rect }
inExpandingRect :: Endo Rect -> Endo ExpandingRect
inExpandingRect f = ExpandingRect . f . unExpandingRect
inExpandingRect2 :: (Rect -> Rect -> Rect) ->
                    ExpandingRect -> ExpandingRect -> ExpandingRect
inExpandingRect2 f = inExpandingRect . f . unExpandingRect
instance Monoid ExpandingRect where
  mempty = ExpandingRect $ Rect (pure maxBoundHack) (pure minBoundHack)
  mappend = inExpandingRect2 $ combineRect2 (liftA2 min) (liftA2 max)

newtype ShrinkingRect = ShrinkingRect { unShrinkingRect :: Rect }
inShrinkingRect :: Endo Rect -> Endo ShrinkingRect
inShrinkingRect f = ShrinkingRect . f . unShrinkingRect
inShrinkingRect2 :: (Rect -> Rect -> Rect) ->
                    ShrinkingRect -> ShrinkingRect -> ShrinkingRect
inShrinkingRect2 f = inShrinkingRect . f . unShrinkingRect
instance Monoid ShrinkingRect where
  mempty = ShrinkingRect $ Rect (pure minBoundHack) (pure maxBoundHack)
  mappend = inShrinkingRect2 $ combineRect2 (liftA2 max) (liftA2 min)
