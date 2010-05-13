{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.VtyWidgets.Widget
    (Placable(..), atPlace, atRequestedSize,
     Display, unDisplay, atImage, expand,
     makeDisplay, clipDisplay,
     Widget(..), atDisplay, atKeymap, requestedSize,
     make, simpleDisplay,
     HasFocus(..))
where

import Data.Monoid(Monoid(..))
import Data.Function.Utils(result, inFlip)
import Graphics.UI.VtyWidgets.SizeRange(SizeRange(..), Size)
import qualified Graphics.UI.VtyWidgets.SizeRange as SizeRange
import Graphics.UI.VtyWidgets.Keymap(Keymap)
import Graphics.UI.VtyWidgets.Rect(Rect(..))
import Graphics.UI.VtyWidgets.TermImage(TermImage)
import qualified Graphics.UI.VtyWidgets.TermImage as TermImage
import Control.Applicative(pure, liftA2)

type Endo a = a -> a

data Placable r = Placable {
  placableRequestedSize :: SizeRange,
  placablePlace :: Size -> r
  }
atRequestedSize :: Endo SizeRange -> Endo (Placable r)
atRequestedSize f d = d{placableRequestedSize = f $ placableRequestedSize d}
atPlace :: ((Size -> r) -> Size -> r') ->
           Placable r -> Placable r'
atPlace f d = d{placablePlace = f $ placablePlace d}
instance Functor Placable where
  fmap = atPlace . result
instance Monoid r => Monoid (Placable r) where
  mempty = Placable mempty mempty
  Placable x1 y1 `mappend` Placable x2 y2 = Placable (x1 `mappend` x2) (y1 `mappend` y2)

type Display imgarg = Placable (imgarg -> TermImage)
unDisplay :: Display imgarg -> (SizeRange, Size -> imgarg -> TermImage)
unDisplay (Placable rs mkImage) = (rs, mkImage)

atImage :: Endo TermImage -> Endo (Display imgarg)
atImage = fmap . result

clipDisplay :: Display imgarg -> Display imgarg
clipDisplay = (atPlace . inFlip . result) clip
  where
    clip mkImage size = TermImage.clip (Rect (pure 0) size) (mkImage size)

makeDisplay :: SizeRange -> (Size -> imgarg -> TermImage) -> Display imgarg
makeDisplay = (result . result) clipDisplay Placable

expand :: Size -> Endo (Display imgarg)
expand extra = (atRequestedSize . SizeRange.atBothSizes . liftA2 (+)) extra .
               (atImage . TermImage.translate . fmap (`div` 2)) extra

newtype HasFocus = HasFocus { hasFocus :: Bool }
  deriving (Show, Read, Eq, Ord)

data Widget k = Widget {
  -- The boundingRect topleft is ignored, and the bottom-right is
  -- considered the size
  widgetDisplay :: Display HasFocus,
  widgetKeymap :: Keymap k
  }
atDisplay :: Endo (Display HasFocus) -> Endo (Widget k)
atDisplay f w = w{widgetDisplay = f (widgetDisplay w)}
atKeymap :: (Keymap a -> Keymap b) ->
            Widget a -> Widget b
atKeymap f w = w{widgetKeymap = f (widgetKeymap w)}

instance Monoid (Widget k) where
  mempty = Widget mempty mempty
  Widget x1 y1 `mappend` Widget x2 y2 = Widget (x1 `mappend` x2) (y1 `mappend` y2)

simpleDisplay :: Display HasFocus -> Widget k
simpleDisplay display = Widget display mempty

make :: SizeRange -> (Size -> HasFocus -> TermImage) -> Keymap k -> Widget k
make sr f = Widget (makeDisplay sr f)

instance Functor Widget where
  fmap = atKeymap . fmap

requestedSize :: Widget k -> SizeRange
requestedSize = placableRequestedSize . widgetDisplay
