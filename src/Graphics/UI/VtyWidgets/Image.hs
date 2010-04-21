{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Image
    (Image, mkImage, pick, translate,
     boundingRect, inBoundingRect)
where

import Data.Monoid(Monoid(..))
import Control.Applicative(Applicative(..), liftA2)
import Control.Compose((:.)(O, unO))
import Control.Arrow((***))
import Graphics.UI.VtyWidgets.Rect(ClipRect(..), Coordinate)
import qualified Graphics.UI.VtyWidgets.Rect as Rect

type Endo a = a -> a

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
           
translate :: Coordinate -> Image a -> Image a
translate c = inImage $ (Rect.inClipRect . Rect.atBoth . liftA2 (+)) c *** (argument . subtract) c
