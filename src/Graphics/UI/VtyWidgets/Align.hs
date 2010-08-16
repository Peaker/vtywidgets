{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Align
    (to)
where

import           Control.Applicative              (liftA2)
import           Data.Function.Utils              (Endo)
import           Data.Vector.Vector2              (Vector2)
import           Graphics.UI.VtyWidgets.SizeRange (SizeRange(..))
import           Graphics.UI.VtyWidgets.Display   (Display)
import qualified Graphics.UI.VtyWidgets.Placable  as Placable
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage

type Alignment = Vector2 Double

to :: Functor f => Alignment -> Endo (Display f)
to alignment display = Placable.atPlace alignImage display
  where
    maxSize = srMaxSize . Placable.pRequestedSize $ display
    alignImage mkImage givenSize = TermImage.translate translation `fmap` mkImage size
      where
        translation = fmap round . liftA2 (*) alignment . fmap fromIntegral $ extraSize
        extraSize = liftA2 (-) givenSize size
        size = liftA2 min givenSize maxSize
