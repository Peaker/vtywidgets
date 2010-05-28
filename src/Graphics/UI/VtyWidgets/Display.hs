{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Display
    (Display, atImage, atImageArg, expand, make, clip)
where

import Control.Applicative(pure, liftA2)
import Data.Function.Utils(Endo, result, argument, inFlip)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.SizeRange(SizeRange, Size)
import Data.Vector.Rect(Rect(..))
import qualified Graphics.UI.VtyWidgets.Placable as Placable
import Graphics.UI.VtyWidgets.Placable(Placable(..))
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Graphics.UI.VtyWidgets.TermImage(TermImage(..))

type Display imgarg = Placable (imgarg -> TermImage)

atImage :: Endo TermImage -> Endo (Display imgarg)
atImage = fmap . result
atImageArg :: (imgarg' -> imgarg) ->
              Display imgarg -> Display imgarg'
atImageArg = fmap . argument

clip :: Endo (Display imgarg)
clip = (Placable.atPlace . inFlip . result) clip'
  where
    clip' mkImage size = TermImage.clip (Rect (pure 0) size) (mkImage size)

make :: SizeRange -> (Size -> imgarg -> TermImage) -> Display imgarg
make = (result . result) clip Placable

expand :: Size -> Endo (Display imgarg)
expand extra = (Placable.atRequestedSize . SizeRange.atBothSizes . liftA2 (+)) extra .
               (atImage . TermImage.translate . fmap (`div` 2)) extra
