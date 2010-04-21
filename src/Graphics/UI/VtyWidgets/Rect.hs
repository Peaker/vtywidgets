{-# OPTIONS -Wall -O2 #-}
module Graphics.UI.VtyWidgets.Rect
    (Coordinate, Rect(..),
     atTopLeft, atBottomRight, atBoth,
     ClipRect(..), inClipRect, inClipRect2,
     IntersectRect(..), inIntersectRect, inIntersectRect2)
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

newtype ClipRect = ClipRect { unClipRect :: Rect }
inClipRect :: Endo Rect -> Endo ClipRect
inClipRect f = ClipRect . f . unClipRect
inClipRect2 :: (Rect -> Rect -> Rect) ->
               ClipRect -> ClipRect -> ClipRect
inClipRect2 f = inClipRect . f . unClipRect
instance Monoid ClipRect where
  mempty = ClipRect $ Rect (pure maxBound) (pure minBound)
  mappend = inClipRect2 $ combineRect2 (liftA2 min) (liftA2 max)

newtype IntersectRect = IntersectRect { unIntersectRect :: Rect }
inIntersectRect :: Endo Rect -> Endo IntersectRect
inIntersectRect f = IntersectRect . f . unIntersectRect
inIntersectRect2 :: (Rect -> Rect -> Rect) ->
               IntersectRect -> IntersectRect -> IntersectRect
inIntersectRect2 f = inIntersectRect . f . unIntersectRect
instance Monoid IntersectRect where
  mempty = IntersectRect $ Rect (pure minBound) (pure maxBound)
  mappend = inIntersectRect2 $ combineRect2 (liftA2 max) (liftA2 min)
