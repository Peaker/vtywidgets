{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Image
    (Image, make, pick, translate,
     boundingRect, atBoundingRect)
where

import Data.Monoid(Monoid(..))
import Control.Applicative(Applicative(..), liftA2)
import Control.Compose((:.)(O, unO))
import Control.Arrow((***))
import Graphics.UI.VtyWidgets.Rect(ExpandingRect(..), Coordinate)
import qualified Graphics.UI.VtyWidgets.Rect as Rect

type Endo a = a -> a

newtype Image a = Image { runImage :: ((,) ExpandingRect :. (->) Coordinate) a }
  deriving (Functor, Applicative)
unImage :: Image a -> (ExpandingRect, Coordinate -> a)
unImage = unO . runImage
inImage :: ((ExpandingRect, Coordinate -> a) ->
            (ExpandingRect, Coordinate -> b)) ->
           Image a -> Image b
inImage f = Image . O . f . unImage
inImage2 :: ((ExpandingRect, Coordinate -> a) ->
             (ExpandingRect, Coordinate -> b) ->
             (ExpandingRect, Coordinate -> c)) ->
            Image a -> Image b -> Image c
inImage2 f = inImage . f . unImage

make :: ExpandingRect -> (Coordinate -> a) -> Image a
make cr f = Image . O . (,) cr $ f

instance Monoid a => Monoid (Image a) where
  mempty = Image . O $ mempty
  mappend = inImage2 $ mappend

boundingRect :: Image a -> ExpandingRect
boundingRect = fst . unImage

atBoundingRect :: Endo ExpandingRect -> Endo (Image a)
atBoundingRect f img = make boundingRect' . pick $ img
  where
    boundingRect' = f . boundingRect $ img

pick :: Image a -> Coordinate -> a
pick = snd . unImage

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

translate :: Coordinate -> Image a -> Image a
translate c = inImage $ (Rect.inExpandingRect . Rect.atBoth . liftA2 (+)) c *** (argument . subtract) c
