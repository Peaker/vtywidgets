{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Image(Image, Coordinate, mkImage,
             pick, boundingRect, translate) where

import Data.Monoid(Monoid(..))
import Vector2(Vector2(..))
import Control.Applicative(Applicative(..), liftA2)
import Control.Compose((:.)(O, unO))
import Control.Arrow((***))
import Control.Monad(join)

type Coordinate = Vector2 Int

data ClipRect = ClipRect { topLeft :: Coordinate,
                           bottomRight :: Coordinate }
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

mkImage :: (Coordinate, Coordinate) -> (Coordinate -> a) -> Image a
mkImage (tl, br) f = Image . O . (,) (ClipRect tl br) $ f

instance Monoid a => Monoid (Image a) where
  mempty = Image . O $ mempty
  mappend = inImage2 $ mappend

boundingRect :: Image a -> (Coordinate, Coordinate)
boundingRect = unClipRect . fst . unImage

pick :: Image a -> Coordinate -> a
pick = snd . unImage

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)
           
translate :: Coordinate -> Image a -> Image a
translate c = inImage $ (inClipRect . both . liftA2 (+)) c *** (argument . subtract) c
