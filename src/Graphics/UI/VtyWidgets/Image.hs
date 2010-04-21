{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Image
    (Image, mkImage, pick, translate,
     Coordinate, ClipRect(..), unClipRect, inClipRect, inClipRect2, inTopLeft, inBottomRight,
     boundingRect, inBoundingRect)
where

import Data.Monoid(Monoid(..))
import Control.Applicative(Applicative(..), liftA2)
import Control.Compose((:.)(O, unO))
import Control.Arrow((***))
import Control.Monad(join)
import Graphics.UI.VtyWidgets.Vector2(Vector2(..))

type Coordinate = Vector2 Int

type Endo a = a -> a

data ClipRect = ClipRect { topLeft :: Coordinate,
                           bottomRight :: Coordinate }
inTopLeft :: Endo Coordinate -> Endo ClipRect
inTopLeft f (ClipRect tl br) = ClipRect (f tl) br
inBottomRight :: Endo Coordinate -> Endo ClipRect
inBottomRight f (ClipRect tl br) = ClipRect tl (f br)

unClipRect :: ClipRect -> (Coordinate, Coordinate)
unClipRect = liftA2 (,) topLeft bottomRight
inClipRect :: ((Coordinate, Coordinate) -> (Coordinate, Coordinate)) ->
              ClipRect -> ClipRect
inClipRect f = uncurry ClipRect . f . unClipRect
inClipRect2 :: ((Coordinate, Coordinate) ->
                (Coordinate, Coordinate) ->
                (Coordinate, Coordinate)) ->
               ClipRect -> ClipRect -> ClipRect
inClipRect2 f = inClipRect . f . unClipRect

instance Monoid ClipRect where
  mempty = ClipRect (pure 0) (pure 0)
  mappend = inClipRect2 (\(tl1, br1) (tl2, br2) -> (liftA2 min tl1 tl2, liftA2 max br1 br2))

newtype Image a = Image { runImage :: ((,) ClipRect :. (->) Coordinate) a }
  deriving (Functor, Applicative)
unImage :: Image a -> (ClipRect, Coordinate -> a)
unImage = unO . runImage
inImage :: ((ClipRect, Coordinate -> a) ->
            (ClipRect, Coordinate -> b)) ->
           Image a -> Image b
inImage f = Image . O . f . unImage
inImage2 :: ((ClipRect, Coordinate -> a) ->
             (ClipRect, Coordinate -> b) ->
             (ClipRect, Coordinate -> c)) ->
            Image a -> Image b -> Image c
inImage2 f = inImage . f . unImage

mkImage :: ClipRect -> (Coordinate -> a) -> Image a
mkImage cr f = Image . O . (,) cr $ f

instance Monoid a => Monoid (Image a) where
  mempty = Image . O $ mempty
  mappend = inImage2 $ mappend

boundingRect :: Image a -> ClipRect
boundingRect = fst . unImage

inBoundingRect :: Endo ClipRect -> Endo (Image a)
inBoundingRect f img = mkImage boundingRect' . pick $ img
  where
    boundingRect' = f . boundingRect $ img

pick :: Image a -> Coordinate -> a
pick = snd . unImage

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)
           
translate :: Coordinate -> Image a -> Image a
translate c = inImage $ (inClipRect . both . liftA2 (+)) c *** (argument . subtract) c
