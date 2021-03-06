{-# OPTIONS -Wall -O2 #-}
module Data.Vector.Rect
    (Coordinate, Rect(..), makeSized, inside, clip, enum,
     atTopLeft, atBottomRight, atBoth, translate,
     ExpandingRect(..), inExpandingRect, inExpandingRect2, withExpandingRect, withExpandingRect2,
     ShrinkingRect(..), inShrinkingRect, inShrinkingRect2, withShrinkingRect, withShrinkingRect2,
    )
where

import Data.Function.Utils (Endo, Endo2)
import Data.Monoid         (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Control.Applicative (pure, liftA2)

type Coordinate = Vector2 Int
data Rect = Rect { topLeft :: Coordinate,
                   bottomRight :: Coordinate }
  deriving (Show, Read, Eq, Ord)
atTopLeft :: Endo Coordinate -> Endo Rect
atTopLeft f (Rect tl br) = Rect (f tl) br
atBottomRight :: Endo Coordinate -> Endo Rect
atBottomRight f (Rect tl br) = Rect tl (f br)
atBoth :: Endo Coordinate -> Endo Rect
atBoth f (Rect tl br) = Rect (f tl) (f br)

makeSized :: Coordinate -> Coordinate -> Rect
makeSized offset size = Rect offset (liftA2 (+) offset size)

translate :: Coordinate -> Endo Rect
translate = atBoth . liftA2 (+)

combineRect2 :: Endo2 Coordinate -> Endo2 Coordinate ->
                Endo2 Rect
combineRect2 ftl fbr (Rect tl1 br1) (Rect tl2 br2) =
  Rect (tl1 `ftl` tl2) (br1 `fbr` br2)

minBoundHack :: (Bounded a, Integral a) => a
minBoundHack = minBound `div` 25

maxBoundHack :: (Bounded a, Integral a) => a
maxBoundHack = maxBound `div` 25

inside :: Coordinate -> Rect -> Bool
inside (Vector2 x y) (Rect (Vector2 l t) (Vector2 r b)) =
  l <= x && x < r &&
  t <= y && y < b

newtype ExpandingRect = ExpandingRect { unExpandingRect :: Rect }
  deriving (Show, Read, Eq, Ord)
inExpandingRect :: Endo Rect -> Endo ExpandingRect
inExpandingRect f = ExpandingRect . f . unExpandingRect
inExpandingRect2 :: Endo2 Rect -> Endo2 ExpandingRect
inExpandingRect2 f = inExpandingRect . f . unExpandingRect
withExpandingRect :: Endo ExpandingRect -> Endo Rect
withExpandingRect f = unExpandingRect . f . ExpandingRect
withExpandingRect2 :: Endo2 ExpandingRect -> Endo2 Rect
withExpandingRect2 f = withExpandingRect . f . ExpandingRect

instance Monoid ExpandingRect where
  mempty = ExpandingRect $ Rect (pure maxBoundHack) (pure minBoundHack)
  mappend = inExpandingRect2 $ combineRect2 (liftA2 min) (liftA2 max)

newtype ShrinkingRect = ShrinkingRect { unShrinkingRect :: Rect }
  deriving (Show, Read, Eq, Ord)
inShrinkingRect :: Endo Rect -> Endo ShrinkingRect
inShrinkingRect f = ShrinkingRect . f . unShrinkingRect
inShrinkingRect2 :: Endo2 Rect -> Endo2 ShrinkingRect
inShrinkingRect2 f = inShrinkingRect . f . unShrinkingRect
withShrinkingRect :: Endo ShrinkingRect -> Endo Rect
withShrinkingRect f = unShrinkingRect . f . ShrinkingRect
withShrinkingRect2 :: Endo2 ShrinkingRect -> Endo2 Rect
withShrinkingRect2 f = withShrinkingRect . f . ShrinkingRect
instance Monoid ShrinkingRect where
  mempty = ShrinkingRect $ Rect (pure minBoundHack) (pure maxBoundHack)
  mappend = inShrinkingRect2 $ combineRect2 (liftA2 max) (liftA2 min)

clip :: Rect -> Rect -> Rect
clip = withShrinkingRect2 mappend

enum :: Rect -> [Coordinate]
enum (Rect (Vector2 l t) (Vector2 r b)) = liftA2 Vector2 xs ys
  where
    xs = takeWhile (<r) [l..]
    ys = takeWhile (<b) [t..]
