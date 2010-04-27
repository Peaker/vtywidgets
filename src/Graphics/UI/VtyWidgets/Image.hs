{-# OPTIONS -Wall -O2 #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}

module Graphics.UI.VtyWidgets.Image
    (Image, make, pick, translate, clip,
     boundingRect, atBoundingRect)
where

import Data.Monoid(Monoid(..))
import Control.Applicative(liftA2)
import Control.Compose((:.)(O, unO))
import Control.Arrow(first, second, (***))
import Graphics.UI.VtyWidgets.Rect(Rect, ExpandingRect(..), Coordinate)
import qualified Graphics.UI.VtyWidgets.Rect as Rect
import Graphics.UI.VtyWidgets.TMap(TMap)
import qualified Graphics.UI.VtyWidgets.TMap as TMap

type Endo a = a -> a

newtype Image a = Image { runImage :: ((,) ExpandingRect :. TMap Coordinate) a }
  deriving (Functor)
unImage :: Image a -> (ExpandingRect, TMap Coordinate a)
unImage = unO . runImage
inImage :: ((ExpandingRect, TMap Coordinate a) ->
            (ExpandingRect, TMap Coordinate b)) ->
           Image a -> Image b
inImage f = Image . O . f . unImage
inImage2 :: ((ExpandingRect, TMap Coordinate a) ->
             (ExpandingRect, TMap Coordinate b) ->
             (ExpandingRect, TMap Coordinate c)) ->
            Image a -> Image b -> Image c
inImage2 f = inImage . f . unImage

atTMap :: (TMap Coordinate a -> TMap Coordinate b) ->
          Image a -> Image b
atTMap = inImage . second

make :: ExpandingRect -> TMap Coordinate a -> Image a
make cr f = Image . O . (,) cr $ f

instance Monoid a => Monoid (Image a) where
  mempty = Image . O $ mempty
  mappend = inImage2 mappend

boundingRect :: Image a -> ExpandingRect
boundingRect = fst . unImage

atBoundingRect :: Endo ExpandingRect -> Endo (Image a)
atBoundingRect = inImage . first

clip :: Monoid a => Rect -> Image a -> Image a
-- TODO: Also clip the ExpandingRect...
clip = atTMap . TMap.filterKeys . flip Rect.inside

pick :: Image a -> Coordinate -> a
pick = flip TMap.lookup . snd . unImage

translate :: Coordinate -> Image a -> Image a
translate c =
  inImage $
  (Rect.inExpandingRect . Rect.atBoth .
   liftA2 (+)) c
  ***
  (TMap.mapKeys .
   liftA2 (+)) c
