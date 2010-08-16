{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Display
    (Display, atImage, atSizedImage, expand, make, clip)
where

import           Control.Applicative              (pure, liftA2)
import           Data.Function.Utils              (Endo, result)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange, Size)
import           Data.Vector.Rect                 (Rect(..))
import qualified Graphics.UI.VtyWidgets.Placable  as Placable
import           Graphics.UI.VtyWidgets.Placable  (Placable(..))
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import           Graphics.UI.VtyWidgets.TermImage (TermImage(..))

type Display f = Placable (f TermImage)

atImage :: Functor f => Endo TermImage -> Endo (Display f)
atImage = fmap . fmap

atSizedImage :: Functor f => (Size -> Endo TermImage) -> Endo (Display f)
atSizedImage f = Placable.atPlace placeFunc
  where
    placeFunc mkFImage size = fmap (f size) $ mkFImage size

clip :: Functor f => Endo (Display f)
clip = atSizedImage (TermImage.clip . Rect (pure 0))

make :: Functor f => SizeRange -> (Size -> f TermImage) -> Display f
make = (result . result) clip Placable

expand :: Functor f => Size -> Endo (Display f)
expand extra = (Placable.atRequestedSize . SizeRange.atBothSizes . liftA2 (+)) extra .
               (atImage . TermImage.translate . fmap (`div` 2)) extra
