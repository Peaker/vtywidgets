{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Placable
    (Placable(..), atPlace, atRequestedSize)
where

import Data.Monoid                      (Monoid(..))
import Data.Function.Utils              (Endo, result)
import Graphics.UI.VtyWidgets.SizeRange (Size, SizeRange)

data Placable r = Placable {
  pRequestedSize :: SizeRange,
  pPlace :: Size -> r
  }
atRequestedSize :: Endo SizeRange -> Endo (Placable r)
atRequestedSize f d = d{pRequestedSize = f $ pRequestedSize d}
atPlace :: ((Size -> r) -> Size -> r') ->
           Placable r -> Placable r'
atPlace f d = d{pPlace = f $ pPlace d}
instance Functor Placable where
  fmap = atPlace . result
instance Monoid r => Monoid (Placable r) where
  mempty = Placable mempty mempty
  Placable x1 y1 `mappend` Placable x2 y2 = Placable (x1 `mappend` x2) (y1 `mappend` y2)
